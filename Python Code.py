# -*- coding: utf-8 -*-
"""
Created on Fri May  2 14:19:42 2025

@author: sdshan99
"""

# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import json
import os
import uuid
import math
import random
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, base
from shapely.affinity import rotate, translate, scale

from shapely.strtree import STRtree
from typing import List, Tuple
import matplotlib.colors as mcolors
import numpy as np
from concurrent.futures import ThreadPoolExecutor
from numba import njit


# --- Configuration ---
ROTATION_ENABLED = True
STRIP_HEIGHT = 110
STRIP_WIDTH = 200
MARGIN = 1.0
JSON_PATH = 'marques.json'
DEFAULT_ROTATIONS = [0, 90, 180, 270]
ENABLE_MIRRORING = True
GRID_STEP = 4  # instead of 2
from itertools import product, cycle
TRANSFORM_MODES = list(product(DEFAULT_ROTATIONS, [False, True] if ENABLE_MIRRORING else [False]))

# --- Load Input ---
with open(JSON_PATH) as f:
    data = json.load(f)
items = data['Items']

# --- Placement Context ---
output_dir = f"placement_output_{uuid.uuid4().hex[:8]}"
os.makedirs(output_dir, exist_ok=True)



# --- Simulated Annealing Components ---

def compute_height(polygons: List[Polygon]) -> float:
    return max((p.bounds[3] for p in polygons), default=0)

def try_transform_and_place(base_shape, angle, mirror, placed_polys, tree):
    rotated = rotate(base_shape, angle, origin=(0, 0))
    if mirror:
        rotated = scale(rotated, xfact=-1, yfact=1, origin='center')
    candidate = try_place_without_overlap(rotated, placed_polys, tree)
    return (candidate, angle, mirror) if candidate else None

def generate_initial_solution(items):
    solution = []
    placed_polys = []
    ref_shapes = []

    for item in items:
        base_shape = Polygon(item['Shape']['Data'])
        base_filename = item['Dxf'].split("/")[-1]  # e.g., i_0.dxf

        for i in range(item['Demand']):
            placed = False

            geom_list = [p[0] for p in placed_polys]
            tree = STRtree(geom_list)

            with ThreadPoolExecutor() as executor:
                transforms = random.sample(get_unique_transforms(base_shape), len(TRANSFORM_MODES))
                futures = [
                    executor.submit(try_transform_and_place, base_shape, angle, mirror, geom_list, tree)
                    for angle, mirror in transforms
                ]

                for future, (angle, mirror) in zip(futures, transforms):
                    result = future.result()
                    if result:
                        candidate, _, _ = result
                        unique_id = f"{base_filename}_{i}"
                        solution.append((candidate, unique_id, angle, mirror))
                        placed_polys.append((candidate, unique_id, angle, mirror))
                        ref_shapes.append((base_shape, unique_id))
                        placed = True
                        break

            if not placed:
                print(f"⚠️ Failed to place copy {i} of {item['Dxf']} with any transform")

    return solution, ref_shapes



def bottom_left_rebuild(solution, ref_shapes):
    sorted_refs = list(zip(solution, ref_shapes))
    random.shuffle(sorted_refs)

    placed = []
    for (_, dxf, _, _), (base_shape, dxf_id) in sorted_refs:
        placed_poly = None
        for angle, mirror in random.sample(get_unique_transforms(base_shape), len(TRANSFORM_MODES)):
            rotated = rotate(base_shape, angle, origin=(0, 0))
            if mirror:
                rotated = scale(rotated, xfact=-1, yfact=1, origin='center')
            minx, miny, maxx, maxy = rotated.bounds
            width = maxx - minx
            height = maxy - miny
    
            x_range = np.arange(0, (STRIP_WIDTH - width) + 1, 2)
            y_range = np.arange(0, (STRIP_HEIGHT - height) + 1, 2)

            for x in x_range:
                for y in y_range:
                    moved = translate(rotated, x - minx, y - miny)
                    if moved.is_valid and all(not moved.intersects(p[0]) for p in placed):
                        placed.append((moved, dxf_id, angle, mirror))
                        placed_poly = moved
                        break
                if placed_poly:
                    break
            if placed_poly:
                break
        if not placed_poly:
            print(f"Failed to re-place: {dxf_id}")
            return None
    return placed


def perturb(current_solution, ref_shapes):
    new_solution = current_solution[:]
    index = random.randint(0, len(new_solution) - 1)

    base_shape, dxf = ref_shapes[index]
    _, _, current_angle, current_mirror = new_solution[index]

    remaining = new_solution[:index] + new_solution[index+1:]
    placed_polys = [p[0] for p in remaining]

    # 70% chance to keep current transform, 30% to try a new one
    if random.random() < 0.7:
        angle, mirror = current_angle, current_mirror
    else:
        angle, mirror = random.choice(TRANSFORM_MODES)

    rotated = rotate(base_shape, angle, origin=(0, 0))
    if mirror:
        rotated = scale(rotated, xfact=-1, yfact=1, origin='center')

    minx, miny, maxx, maxy = rotated.bounds
    shape_width = maxx - minx
    shape_height = maxy - miny

    for x in range(0, int(STRIP_WIDTH - shape_width) + 1, 2):
        for y in range(0, int(STRIP_HEIGHT - shape_height) + 1, 2):
            moved = translate(rotated, x - minx, y - miny)
            if moved.is_valid and all(not moved.intersects(p) for p in placed_polys):
                return remaining[:index] + [(moved, dxf, angle, mirror)] + remaining[index:]

    return current_solution  # fallback

def is_valid(solution):
    geoms = [p[0] for p in solution if isinstance(p[0], base.BaseGeometry) and p[0].is_valid]
    str_tree = STRtree(geoms)
    for poly in geoms:
        if not isinstance(poly, base.BaseGeometry) or not poly.is_valid:
            continue
        for other in str_tree.query(poly):
            if (
                isinstance(other, base.BaseGeometry) and
                poly != other and
                other.is_valid and
                poly.intersects(other)
            ):
                return False
        minx, miny, maxx, maxy = poly.bounds
        if not (0 <= minx <= STRIP_WIDTH and maxx <= STRIP_WIDTH):
            return False
        if not (0 <= miny <= STRIP_HEIGHT and maxy <= STRIP_HEIGHT):
            return False
    return True

def try_place_without_overlap(rotated_poly, placed_polys, tree=None):
    r_minx, r_miny, r_maxx, r_maxy = rotated_poly.bounds
    shape_width = r_maxx - r_minx
    shape_height = r_maxy - r_miny

    # Filter geometries once, and cache STRtree once
    geoms = [p for p in placed_polys if isinstance(p, base.BaseGeometry) and p.is_valid]
    if not geoms:
        return translate(rotated_poly, 0 - r_minx, 0 - r_miny)

    if tree is None:
        tree = STRtree(geoms)

    x_range = np.arange(0, STRIP_WIDTH - shape_width + 1, 2)
    y_range = np.arange(0, STRIP_HEIGHT - shape_height + 1, 2)

    for x in x_range:
        for y in y_range:
            moved = translate(rotated_poly, x - r_minx, y - r_miny)

            candidates = tree.query(moved)

            if all(
                isinstance(other, base.BaseGeometry) and
                moved.is_valid and
                not moved.intersects(other)
                for other in candidates
            ):
                return moved

    return None

def get_unique_transforms(base_shape):
    seen = set()
    unique = []
    for angle, mirror in TRANSFORM_MODES:
        transformed = rotate(base_shape, angle, origin=(0, 0))
        if mirror:
            transformed = scale(transformed, xfact=-1, yfact=1, origin='center')
        hash_val = hash(transformed.wkt)
        if hash_val not in seen:
            seen.add(hash_val)
            unique.append((angle, mirror))
    return unique


def simulated_annealing(items, max_iter=1000, start_temp=1000.0, alpha=0.995, init_solution=None):
    if init_solution:
        current = init_solution
        # Recover base shapes using original items
        dxf_to_shape = {item['Dxf'].split('/')[-1]: Polygon(item['Shape']['Data']) for item in items}
        ref_shapes = []
        for _, dxf_id, _, _ in current:
            base_id = "_".join(dxf_id.split("_")[:2])
            ref_shapes.append((dxf_to_shape[base_id], dxf_id))
    else:
        current, ref_shapes = generate_initial_solution(items)

    best = current
    temp = start_temp


    def fitness(polys):
        return compute_width([p[0] for p in polys])  # lower = better
    
    def layout_keys(layout):
        return sorted((dxf for _, dxf, _, _ in layout))
    
    for iteration in range(max_iter):
        if iteration % 200 == 0 and layout_keys(current) != layout_keys(best):
            neighbor = bottom_left_rebuild(current, ref_shapes)
        else:
            neighbor = perturb(current, ref_shapes)
        
        
        
        if (
            neighbor is None or
            not is_valid(neighbor) or
            layout_keys(neighbor) != layout_keys(current)
        ):
            continue
              
        # Detect stagnation every 300 iterations
        if iteration > 0 and iteration % 300 == 0:
            print("No improvement. Trying a full rebuild.")
            rebuilt = bottom_left_rebuild(best, ref_shapes)
            if (
                rebuilt is not None and
                is_valid(rebuilt) and
                layout_keys(rebuilt) == layout_keys(best)
            ):
                current = rebuilt  # try a fresh arrangement
  

        curr_score = fitness(current)
        neigh_score = fitness(neighbor)
        delta = neigh_score - curr_score

        if delta < 0 or random.random() < math.exp(-delta / temp):
            current = neighbor
            if neigh_score < fitness(best):
                best = neighbor

        temp *= alpha
        if iteration % 50 == 0:
            print(f"Iter {iteration} | Temp: {temp:.2f} | Used Width: {fitness(best):.2f}")
            save_iteration_plot(iteration // 50, best, color_map)

    print(f"\nFinal Used Width: {fitness(best):.2f} out of {STRIP_WIDTH}")
    return best

def fitness(polys):
    shapes = [p[0] for p in polys]
    width = compute_width(shapes)
    area_used = sum(p.area for p in shapes)
    area_bbox = width * STRIP_HEIGHT
    white_space_penalty = (area_bbox - area_used) / area_bbox
    return width + white_space_penalty * 10  # weight the penalty

@njit
def compute_max_x(bounds_array):
    max_x = 0.0
    for i in range(bounds_array.shape[0]):
        max_x = max(max_x, bounds_array[i, 2])  # index 2 is max_x
    return max_x


def compute_width(polygons):
    # Pre-extract bounds for Numba
    bounds_array = np.array([p.bounds for p in polygons])
    return compute_max_x(bounds_array)


def save_iteration_plot(index, placements: List[Tuple[Polygon, str, float, bool]], color_map: dict):

    fig, ax = plt.subplots()
    seen_labels = set()
    for poly, dxf, angle, mirror in placements:
        x, y = poly.exterior.xy
        base_id = "_".join(dxf.split("_")[:2])  # e.g., 'i_0.dxf'
        label = base_id if base_id not in seen_labels else None
        label = f"{base_id} ({angle}°, {'M' if mirror else 'N'})" if base_id not in seen_labels else None

        ax.fill(x, y, color=color_map.get(base_id, "gray"), alpha=0.5, label=label)
        seen_labels.add(base_id)
        ax.plot(x, y, color='black', linewidth=0.5)

    ax.set_xlim(0, STRIP_WIDTH)
    ax.set_ylim(0, STRIP_HEIGHT)
    ax.set_aspect('equal')
    ax.set_title(f"Step {index}: Irregular Shape Placement")
    #ax.legend(loc='upper right', fontsize='x-small')
    #plt.savefig(os.path.join(output_dir, f"step_{index:03d}.png"))
    plt.savefig(os.path.join(output_dir, f"step_{index}.png"))

    plt.close()


# --- Genetic Algorithm Hybrid ---

def initialize_population(items, pop_size=20):
    all_copies = []
    for item in items:
        base_id = item['Dxf'].split('/')[-1]  # e.g., 'i_0.dxf'
        for i in range(item['Demand']):
            uid = f"{base_id}_{i}"
            angle, mirror = random.choice(TRANSFORM_MODES)
            all_copies.append((base_id, uid, angle, mirror))

    population = []
    for _ in range(pop_size):
        genome = random.sample(all_copies, len(all_copies))  # shuffled layout order
        population.append(genome)

    return population

def place_genome(genome, items):
    # Build a mapping from dxf file to base shape
    dxf_to_shape = {
        item['Dxf'].split('/')[-1]: Polygon(item['Shape']['Data'])
        for item in items
    }

    ref_shapes = []
    for dxf_id, uid, angle, mirror in genome:
        base_shape = dxf_to_shape[dxf_id]
        ref_shapes.append((base_shape, uid, [(angle, mirror)]))

    # Use your existing bottom-left rebuilder
    layout = bottom_left_rebuild([], ref_shapes)
    return layout


def place_individual(individual):
    solution = []
    placed_polys = []

    for base_shape, dxf, uid, angle, mirror in individual:
        rotated = rotate(base_shape, angle, origin=(0, 0))
        if mirror:
            rotated = scale(rotated, xfact=-1, yfact=1, origin='center')

        candidate = try_place_without_overlap(rotated, [p[0] for p in placed_polys])
        if candidate:
            solution.append((candidate, uid, angle, mirror))
            placed_polys.append((candidate, uid, angle, mirror))
        else:
            return None  # Invalid solution
    return solution

def evaluate_individual(individual):
    placed = place_individual(individual)
    if not placed:
        return float('inf'), None
    return fitness(placed), placed


def crossover(parent1, parent2):
    size = len(parent1)
    start, end = sorted(random.sample(range(size), 2))

    middle = parent1[start:end]
    middle_uids = {g[1] for g in middle}

    # Fill rest from parent2 in order, skipping those already in middle
    tail = [g for g in parent2 if g[1] not in middle_uids]

    child = tail[:start] + middle + tail[start:]
    return child

def mutate(genome, angle_choices=[0, 90, 180, 270], mutation_rate=0.1):
    genome = genome[:]
    n = len(genome)

    # 🔁 1. Transform Mutation
    for i in range(n):
        if random.random() < mutation_rate:
            shape, uid, angle, mirror = genome[i]
            new_angle = random.choice([a for a in angle_choices if a != angle])
            new_mirror = random.choice([True, False]) if random.random() < 0.5 else mirror
            genome[i] = (shape, uid, new_angle, new_mirror)

    # 🔀 2. Swap Mutation
    if random.random() < 0.3:
        i, j = random.sample(range(n), 2)
        genome[i], genome[j] = genome[j], genome[i]

    return genome



def genetic_optimizer(items, generations=10, pop_size=20, elite_size=3, color_map=None):
    population = initialize_population(items, pop_size)
    best_layout = None
    best_fitness = float('inf')

    for gen in range(generations):
        scored = []
        for genome in population:
            layout = place_genome(genome, items)
            if layout is None or len(layout) < len(genome):
                score = float('inf')
            else:
                score = fitness(layout)
            scored.append((score, genome, layout))

        scored.sort(key=lambda x: x[0])
        elites = scored[:elite_size]

        print(f"[Gen {gen}] Best Width: {elites[0][0]:.2f}")
        if color_map and elites[0][2]:
            save_iteration_plot(f"gen_{gen:02}", elites[0][2], color_map)

        if elites[0][0] < best_fitness:
            best_fitness = elites[0][0]
            best_layout = elites[0][2]

        # Generate next generation
        next_gen = [e[1] for e in elites]  # Only genomes
        while len(next_gen) < pop_size:
            p1, p2 = random.sample(elites, 2)
            child = crossover(p1[1], p2[1])
            child = mutate(child)
            next_gen.append(child)

        population = next_gen

    # Final polish with SA
    print("Polishing final GA result with Simulated Annealing...")
    final = simulated_annealing(items, init_solution=best_layout)
    return final


# Build base color map once from all pattern names
all_dxf_base_ids = sorted(set(item['Dxf'].split("/")[-1] for item in items))

color_list = list(mcolors.TABLEAU_COLORS.values()) + list(mcolors.CSS4_COLORS.values())

distinct_colors = cycle([
    "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
    "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
    "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000",
    "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080"
])
color_map = {base_id: next(distinct_colors) for base_id in all_dxf_base_ids}


# --- Run Optimization ---
#placements = simulated_annealing(items, max_iter=5000, start_temp=2000, alpha=0.998)
#placements = genetic_optimizer(items, generations=10, pop_size=10, elite_size=3)
placements = genetic_optimizer(items, generations=20, pop_size=20, elite_size=3, color_map=color_map)

final_width = compute_width([p[0] for p in placements])
efficiency = 100.0 * final_width / STRIP_WIDTH
print(f"Efficiency: {efficiency:.2f}% of available width used.")

used_width = compute_width([p[0] for p in placements])
used_height = STRIP_HEIGHT
used_area = used_width * used_height

shape_area = sum(p[0].area for p in placements)
effective_efficiency = 100.0 * shape_area / used_area

print("\n--- Final Metrics ---")
print(f"Used Width: {used_width:.2f} / {STRIP_WIDTH}")
print(f"Total Shape Area: {shape_area:.2f}")
print(f"Used Area: {used_area:.2f}")
print(f"Effective Efficiency (within used region): {effective_efficiency:.2f}%")

expected_count = sum(item["Demand"] for item in items)
actual_count = len(placements)

if actual_count != expected_count:
    print(f"\n⚠️ WARNING: {expected_count - actual_count} shape(s) were lost during optimization.")
else:
    print(f"\n✅ All {actual_count} shapes successfully placed.")

# --- Save Final Output ---
save_iteration_plot(len(placements), placements, color_map)
print(f"Final height used: {compute_height([p[0] for p in placements]):.2f}")