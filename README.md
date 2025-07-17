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

The `requirements.txt` file lists the core packages:

- pandas
- matplotlib
- python-docx

## Usage

Run the analysis script on a CSV file. The script is tailored for tabular
medical research data where each row represents a patient and columns contain
demographic, medical, or cognitive measurements. It cleans the data,
computes summary statistics, generates histograms for numeric variables and
bar charts for categorical variables, and writes the results to a docx file
with nicely formatted tables.

```bash
python scripts/analyze.py path/to/data.csv output_directory
```

The resulting `report.docx` will appear in `output_directory`.

Example using the provided sample dataset:

```bash
python scripts/analyze.py data/sample.csv output
```

## Web interface

A simple Next.js app in `web/` lets you upload CSV files and download a generated report.

```bash
cd web
npm install
npm run dev
```

Open `http://localhost:3000` and upload a CSV file to run the analysis and download `report.docx`.
