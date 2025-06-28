import argparse
import os
from typing import Dict

import pandas as pd
import matplotlib.pyplot as plt
from docx import Document


def load_data(file_path: str) -> pd.DataFrame:
    """Load a CSV file into a DataFrame."""
    return pd.read_csv(file_path)


def clean_data(df: pd.DataFrame) -> pd.DataFrame:
    """\
    Basic cleaning tailored for medical research data.

    - Drops duplicate rows.
    - Attempts to convert columns to numeric when possible.
    - Fills missing values: numeric columns get the median, others get the mode.
    """

    df = df.drop_duplicates()

    # Try to convert columns that look numeric
    for col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="ignore")

    for col in df.columns:
        if pd.api.types.is_numeric_dtype(df[col]):
            df[col] = df[col].fillna(df[col].median())
        else:
            mode = df[col].mode()
            df[col] = df[col].fillna(mode.iloc[0] if not mode.empty else "missing")

    return df


def analyze_data(df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """\
    Generate descriptive statistics for medical datasets.

    Returns a dictionary with numeric summaries, categorical counts, and a
    correlation matrix for numeric variables.
    """

    summaries: Dict[str, pd.DataFrame] = {}

    numeric = df.select_dtypes(include="number")
    if not numeric.empty:
        summaries["numeric"] = numeric.describe().T
        summaries["correlation"] = numeric.corr()

    categorical_columns = df.select_dtypes(exclude="number").columns
    categorical_summary = {}
    for col in categorical_columns:
        categorical_summary[col] = df[col].value_counts().to_frame(name="count")
    if categorical_summary:
        summaries["categorical"] = pd.concat(categorical_summary, axis=0)

    return summaries


def generate_visualization(df: pd.DataFrame, output_path: str) -> None:
    """Create histograms for numeric columns and save as an image."""
    numeric = df.select_dtypes(include="number")
    if numeric.empty:
        return

    numeric.hist(figsize=(10, 8))
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()


def export_to_docx(
    summaries: Dict[str, pd.DataFrame], figure_path: str, output_docx: str
) -> None:
    """Save summary statistics and a figure to a docx file."""
    document = Document()
    document.add_heading("Analysis Summary", level=1)

    numeric = summaries.get("numeric")
    if numeric is not None:
        document.add_heading("Numeric Variables", level=2)
        document.add_paragraph(numeric.to_string())

    categorical = summaries.get("categorical")
    if categorical is not None:
        document.add_heading("Categorical Variables", level=2)
        document.add_paragraph(categorical.to_string())

    correlation = summaries.get("correlation")
    if correlation is not None:
        document.add_heading("Correlation Matrix", level=2)
        document.add_paragraph(correlation.to_string())

    if os.path.exists(figure_path):
        document.add_picture(figure_path)

    document.save(output_docx)


def main(csv_file: str, output_dir: str) -> None:
    """Run the analysis pipeline on `csv_file` and save outputs."""
    if not os.path.isfile(csv_file):
        raise FileNotFoundError(csv_file)

    os.makedirs(output_dir, exist_ok=True)

    df = load_data(csv_file)
    df = clean_data(df)
    summaries = analyze_data(df)

    figure_path = os.path.join(output_dir, "figure.png")
    generate_visualization(df, figure_path)

    docx_path = os.path.join(output_dir, "report.docx")
    export_to_docx(summaries, figure_path, docx_path)

    print(f"Report saved to {docx_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Analyze tabular medical research data and generate a docx report"
    )
    parser.add_argument("csv_file", help="Path to input CSV file")
    parser.add_argument("output_dir", help="Directory to save outputs")
    args = parser.parse_args()
    main(args.csv_file, args.output_dir)
