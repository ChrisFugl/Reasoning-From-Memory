const path = require('path');
const HtmlWebPackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './web/index.js',
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      },
      {
        test: /\.html$/,
        use: [
          {
            loader: 'html-loader'
          }
        ]
      }
    ]
  },
  plugins: [
    new HtmlWebPackPlugin({
      template: './web/index.html',
      filename: './index.html'
    })
  ],
  resolve: {
    alias: {
      web: path.resolve(__dirname, 'web/js/'),
    },
    extensions: ['.js', '.jsx'],
  },
};
