module.exports = {
  mode: "jit",
  content: ["./public/index.html", "./src/**/*.purs"],
  theme: {
    extend: {
      fontFamily: {
        roboto: ["Roboto Mono", "monospace"],
        bebasNeue: ["Bebas Neue", "sans-serif"],
        ubuntuMono: ["Ubuntu Mono", "monospace"],
        inconsolata: ["Inconsolata", "monospace"],
      },
    },
  },
};
