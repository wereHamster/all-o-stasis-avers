const {DefinePlugin} = require('webpack')
const {CheckerPlugin} = require('awesome-typescript-loader')
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: {
    detail: ['whatwg-fetch', './src/main.ts']
  },

  output: {
    publicPath: '/',
    path: __dirname + '/dist',
    filename: '[name].js'
  },

  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.jsx']
  },

  devtool: 'source-map',

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: 'awesome-typescript-loader'
      },
      {
        test: /\.md$/,
        loaders: [require.resolve('catalog/loader'), require.resolve('raw-loader')]
      }
    ]
  },

  plugins: [
    new CheckerPlugin(),
    new HtmlWebpackPlugin({
      inject: true,
      template: 'assets/index.html'
    }),
  ],

  devServer: {
    contentBase: __dirname + '/assets',
    historyApiFallback: true,
  },
}
