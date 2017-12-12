const {optimize, HashedModuleIdsPlugin, DefinePlugin} = require('webpack')
const {CheckerPlugin} = require('awesome-typescript-loader')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const Visualizer = require('webpack-visualizer-plugin')

module.exports = {
  entry: {
    vendor: ['avers', 'react', 'react-dom', 'react-datepicker', 'moment', 'vega-lite', 'vega-parser', 'vega-scenegraph', 'vega-transforms', 'vega-dataflow', 'vega-expression'],
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
      },
      {
        test: /\.svg$/,
        loader: 'svg-react-loader',
      },
    ]
  },

  plugins: [
    new CheckerPlugin(),
    new HashedModuleIdsPlugin(),
    new optimize.CommonsChunkPlugin({name: 'vendor', minChunks: Infinity}),
    new optimize.CommonsChunkPlugin({name: 'main', async: true, children: true}),
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
