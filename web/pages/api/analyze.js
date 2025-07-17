import { IncomingForm } from 'formidable';
import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

export const config = {
  api: {
    bodyParser: false,
  },
};

export default async function handler(req, res) {
  if (req.method !== 'POST') {
    res.status(405).send('Method not allowed');
    return;
  }

  const form = new IncomingForm({ uploadDir: 'tmp', keepExtensions: true });
  const { files } = await new Promise((resolve, reject) => {
    form.parse(req, (err, fields, files) => {
      if (err) reject(err);
      else resolve({ fields, files });
    });
  });

  const file = files.file;
  if (!file) {
    res.status(400).send('No file uploaded');
    return;
  }

  const baseName = path.basename(file.newFilename, path.extname(file.newFilename));
  const outputDir = path.join('tmp', baseName);
  await fs.promises.mkdir(outputDir, { recursive: true });

  const repoRoot = path.join(process.cwd(), '..');
  const script = path.join(repoRoot, 'scripts', 'analyze.py');
  const child = spawn('python', [script, file.filepath, path.join(process.cwd(), outputDir)], { cwd: repoRoot });

  child.on('close', code => {
    if (code !== 0) {
      res.status(500).send('Failed to analyze file');
      return;
    }
    const docPath = path.join(outputDir, 'report.docx');
    res.setHeader('Content-Type', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document');
    res.setHeader('Content-Disposition', 'attachment; filename=report.docx');
    fs.createReadStream(docPath).pipe(res);
  });
}
