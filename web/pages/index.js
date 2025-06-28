import { useState } from 'react';

export default function Home() {
  const [file, setFile] = useState(null);
  const [loading, setLoading] = useState(false);
  const [reportUrl, setReportUrl] = useState('');

  const handleSubmit = async (e) => {
    e.preventDefault();
    if (!file) return;
    const formData = new FormData();
    formData.append('file', file);

    setLoading(true);
    const res = await fetch('/api/analyze', {
      method: 'POST',
      body: formData,
    });
    setLoading(false);

    if (res.ok) {
      const blob = await res.blob();
      const url = window.URL.createObjectURL(blob);
      setReportUrl(url);
    } else {
      alert('Analysis failed');
    }
  };

  return (
    <div style={{ padding: '2rem' }}>
      <h1>Medical Data Analyzer</h1>
      <form onSubmit={handleSubmit}>
        <input type="file" accept=".csv" onChange={e => setFile(e.target.files[0])} />
        <button type="submit">Analyze</button>
      </form>
      {loading && <p>Analyzing...</p>}
      {reportUrl && (
        <p><a href={reportUrl} download="report.docx">Download report</a></p>
      )}
    </div>
  );
}
