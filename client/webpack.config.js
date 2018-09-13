const { CheckerPlugin } = require("awesome-typescript-loader");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const Visualizer = require("webpack-visualizer-plugin");

module.exports = {
  mode: "development",

  entry: {
    vendor: [
      "avers",
      "react",
      "react-dom",
      "react-datepicker",
      "moment",
      "vega-lite",
      "vega-parser",
      "vega-scenegraph",
      "vega-transforms",
      "vega-dataflow",
      "vega-expression"
    ],
    main: ["./src/main.ts"]
  },

  output: {
    publicPath: "/",
    path: __dirname + "/dist",
    filename: "[name].[chunkhash].js"
  },

  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"]
  },

  devtool: "source-map",

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader",
        options: {
          useBabel: true
        }
      },
      {
        test: /\.md$/,
        loaders: [require.resolve("catalog/loader"), "raw-loader"]
      },
      {
        test: /\.svg$/,
        loader: "svg-react-loader"
      }
    ]
  },

  plugins: [
    new CheckerPlugin(),
    new HtmlWebpackPlugin({
      inject: true,
      template: "assets/index.html"
    }),
    new Visualizer()
  ],

  devServer: {
    contentBase: __dirname + "/assets",
    historyApiFallback: true
  }
};
