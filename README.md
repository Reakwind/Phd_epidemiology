# Phd_epidemiology
Analyzing data for an epidemiology PhD.

## Project structure

```
data/          # place raw CSV files here
scripts/       # analysis code
requirements.txt
```

## Setup

Create a Python virtual environment and install dependencies:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Usage

Run the analysis script on a CSV file. The script cleans the data, generates
summary statistics and a simple visualization, and writes everything to a docx
file.

```bash
python scripts/analyze.py path/to/data.csv output_directory
```

The resulting `report.docx` will appear in `output_directory`.
