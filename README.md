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

Run the analysis script on a CSV file. The script is tailored for tabular
medical research data where each row represents a patient and columns contain
demographic, medical, or cognitive measurements. It cleans the data,
computes summary statistics, creates histograms for numeric variables, and
writes everything to a docx file.

```bash
python scripts/analyze.py path/to/data.csv output_directory
```

The resulting `report.docx` will appear in `output_directory`.

Example using the provided sample dataset:

```bash
python scripts/analyze.py data/sample.csv output
```
