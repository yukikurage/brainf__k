const path = require('path');

module.exports = {
  entry: `./entry.js`,

  experiments: {
    topLevelAwait: true,
  },
  output: {
    filename: "index.js",

    path: path.join(__dirname, 'public')
  }
};
