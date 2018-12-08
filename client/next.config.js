const withTypescript = require("@zeit/next-typescript");

module.exports = withTypescript({
  webpack: config => {
    config.node = {
      fs: "empty"
    };

    config.module.rules.push({
      test: /\.svg$/,
      loader: "svg-react-loader"
    });
    config.module.rules.push({
      test: /\.md$/,
      use: ["@catalog/loader", "raw-loader"]
    });

    return config;
  }
});
