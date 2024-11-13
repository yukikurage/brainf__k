import esbuild from "esbuild"

esbuild.build({
  entryPoints: ["entry.js"],
  outfile: "public/index.js",
  bundle: true,
  platform: "node",
  format: "esm",
  target: "esnext",
  minify: true,
  sourcemap: false,
  treeShaking: true,
});