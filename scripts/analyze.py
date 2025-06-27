import argparse
import os
import pandas as pd
import matplotlib.pyplot as plt
from docx import Document


def load_data(file_path: str) -> pd.DataFrame:
    """Load a CSV file into a DataFrame."""
    return pd.read_csv(file_path)


def clean_data(df: pd.DataFrame) -> pd.DataFrame:
    """Simple cleaning: drop duplicates and forward-fill missing values."""
    df = df.drop_duplicates()
    return df.fillna(method="ffill")


def analyze_data(df: pd.DataFrame) -> pd.DataFrame:
    """Generate descriptive statistics."""
    return df.describe(include="all")


def generate_visualization(df: pd.DataFrame, output_path: str) -> None:
    """Create histograms for numeric columns and save as an image."""
    df.hist(figsize=(10, 8))
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()


def export_to_docx(
    summary: pd.DataFrame, figure_path: str, output_docx: str
) -> None:
    """Save summary statistics and a figure to a docx file."""
    document = Document()
    document.add_heading("Analysis Summary", level=1)
    document.add_paragraph(summary.to_string())
    document.add_picture(figure_path)
    document.save(output_docx)


def main(csv_file: str, output_dir: str) -> None:
    df = load_data(csv_file)
    df = clean_data(df)
    summary = analyze_data(df)
    figure_path = os.path.join(output_dir, "figure.png")
    generate_visualization(df, figure_path)
    docx_path = os.path.join(output_dir, "report.docx")
    export_to_docx(summary, figure_path, docx_path)
    print(f"Report saved to {docx_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Analyze CSV data and generate a docx report"
    )
    parser.add_argument("csv_file", help="Path to input CSV file")
    parser.add_argument("output_dir", help="Directory to save outputs")
    args = parser.parse_args()
    main(args.csv_file, args.output_dir)
