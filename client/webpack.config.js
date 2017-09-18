const {optimize, HashedModuleIdsPlugin, DefinePlugin} = require('webpack')
const {CheckerPlugin} = require('awesome-typescript-loader')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const Visualizer = require('webpack-visualizer-plugin')

module.exports = {
  entry: {
    vendor: ['avers', 'react', 'react-dom'],
    main: ['whatwg-fetch', './src/main.ts'],
  },

  output: {
    publicPath: '/',
    path: __dirname + '/dist',
    filename: '[name].[chunkhash].js'
  },

  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.jsx']
  },

  devtool: 'source-map',

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: 'awesome-typescript-loader',
        options: {
        },
      },
      {
        test: /\.md$/,
        loaders: [require.resolve('catalog/loader'), require.resolve('raw-loader')]
      }
    ]
  },

  plugins: [
    new CheckerPlugin(),
    new HashedModuleIdsPlugin(),
    new optimize.CommonsChunkPlugin({name: "vendor"}),
    new optimize.CommonsChunkPlugin({name: "commons"}),
    new HtmlWebpackPlugin({
      inject: true,
      template: 'assets/index.html'
    }),
    new Visualizer(),
  ],

  devServer: {
    contentBase: __dirname + '/assets',
    historyApiFallback: true,
  },
}
