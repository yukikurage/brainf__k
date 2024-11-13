import esbuild from "esbuild"

async function watch() {
  let ctx = await esbuild.context({
    entryPoints: ["entry.js"],
    outfile: "public/index.js",
    bundle: true,
    platform: "node",
    format: "esm",
    target: "esnext",
    minify: false,
    sourcemap: false,
    treeShaking: false,
  });
  await ctx.watch();
  console.log('Watching...');
}

watch();