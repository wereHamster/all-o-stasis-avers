const { CheckerPlugin } = require("awesome-typescript-loader");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const Visualizer = require("webpack-visualizer-plugin");

module.exports = {
  mode: "development",

  entry: {
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
        test: /\.jsx?$/,
        include: /node_modules/,
        exclude: /(@babel\/standalone|react|react-dom|date-fns)/,
        loader: "babel-loader"
      },
      {
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader",
        options: {
          useBabel: true,
          babelCore: "@babel/core"
        }
      },
      {
        test: /\.md$/,
        loaders: ["@catalog/loader", "raw-loader"]
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

  optimization: {
    splitChunks: {
      cacheGroups: {
        vendor: {
          test: /node_modules/,
          name: "vendor",
          chunks: "initial",
          minChunks: 1
        },
        vega: {
          test: /vega/,
          name: "vega",
          chunks: "async",
          minChunks: 1
        },
        catalog: {
          test: /node_modules/,
          name: "catalog",
          chunks: "async",
          minChunks: 2
        }
      }
    }
  },

  devServer: {
    contentBase: __dirname + "/assets",
    historyApiFallback: true
  }
};
