module.exports = {
  typescript: {
    ignoreDevErrors: true,
    ignoreBuildErrors: true
  },

  webpack: config => {
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
};
