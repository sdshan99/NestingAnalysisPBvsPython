# NestingAnalysisPBvsPython
*Comparing heuristic approaches to 2‑D irregular‑shape placement in **PowerBASIC** vs **Python***  

<p align="center">
  <img src="Execution Time by Implementation and Parallelism.png" alt="Runtime comparison plot" width="75%">
</p>

## Project Overview
Manufacturers in garment, leather, and sheet‑metal industries care deeply about **cut‑nesting efficiency**—even a 1 % reduction in scrap can translate into millions of dollars saved.  
Legacy CAD suites still rely on a 1990s **PowerBASIC** solver that is fast but hard to extend. This repo:

1. **Ports** the same meta‑heuristic (Simulated Annealing + Ruin‑&‑Recreate) to modern **Python 3.11**.  
2. **Benchmarks** speed, memory, and search intensity across both implementations under single‑ and multi‑core settings.  
3. **Visualises** the results in publication‑ready charts.

Our ultimate goal is to provide data that will convince stakeholders that a staged migration to Python is feasible without sacrificing production‑line performance.

---

## 🗂 Repository Contents

| File / Folder | Description |
|---------------|-------------|
| **`PB Code.bas`** | Original PowerBASIC implementation (16‑bit, runs under DOSBOX or QB64). |
| **`Python Code.py`** | Clean, self‑contained Python rewrite (uses only `numpy`, `shapely`, `matplotlib`). |
| **`Execution Time by Implementation and Parallelism.png`** | Bar chart comparing wall‑clock times. |
| **`Iterations by Implementation and Parallelism.png`** | Bar chart of search iterations completed. |
| **`Memory Usage by Implementation and Parallelism.png`** | Bar chart of peak RSS memory. |
| **`Vertices Processed by Implementation and Parallelism.png`** | Workload metric (millions of polygon‑vertices handled). |
| **`marques.json`** | Mini dataset of polygon coordinates (for quick smoke tests). |
| **Demo Video** | Short run‑through of code & plots → [Google Drive link](https://drive.google.com/file/d/1OVAJmVURrDDMjwRg2z51QviWX2ZU-Fff/view?usp=sharing). |

---

##  Quick Start (Python ≥ 3.11)

```bash
# 1. Clone the repo
git clone https://github.com/<your‑org>/NestingAnalysisPBvsPython.git
cd NestingAnalysisPBvsPython

# 2. Create a fresh environment (optional but recommended)
python -m venv .venv
source .venv/bin/activate          # Linux/macOS
# .venv\Scripts\activate           # Windows

# 3. Install dependencies
pip install -r requirements.txt    # numpy, shapely, matplotlib

# 4. Run the benchmark
python "Python Code.py" --cores 4  --dataset marques.json
