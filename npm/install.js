#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const https = require("https");
const { execFileSync } = require("child_process");

const REPO = process.env.MORPH_REPO || "aryabyte21/morph";
const VERSION = process.env.MORPH_VERSION || `v${require("./package.json").version}`;

const platformMap = {
  "darwin-arm64": "darwin-arm64",
  "darwin-x64": "darwin-amd64",
  "linux-arm64": "linux-arm64",
  "linux-x64": "linux-amd64",
};

const key = `${process.platform}-${process.arch}`;
const target = platformMap[key];
if (!target) {
  console.error(`morph: no prebuilt binary for ${key}`);
  process.exit(1);
}

const url = `https://github.com/${REPO}/releases/download/${VERSION}/morph-${target}.tar.gz`;
const binDir = path.join(__dirname, "bin");
const archive = path.join(__dirname, "morph.tar.gz");
fs.mkdirSync(binDir, { recursive: true });

function follow(u, cb) {
  https.get(u, (res) => {
    if (res.statusCode >= 300 && res.statusCode < 400 && res.headers.location) {
      follow(res.headers.location, cb);
      return;
    }
    if (res.statusCode !== 200) {
      console.error(`morph: download failed (${res.statusCode}): ${u}`);
      process.exit(1);
    }
    cb(res);
  }).on("error", (err) => {
    console.error(`morph: download error: ${err.message}`);
    process.exit(1);
  });
}

console.log(`morph: downloading ${url}`);
follow(url, (res) => {
  const out = fs.createWriteStream(archive);
  res.pipe(out);
  out.on("finish", () => {
    out.close();
    execFileSync("tar", ["-xzf", archive, "-C", binDir], { stdio: "inherit" });
    fs.unlinkSync(archive);
    fs.chmodSync(path.join(binDir, "morph"), 0o755);
    console.log("morph: installed");
  });
});
