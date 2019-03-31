import React from 'react';
import ReactDOM from 'react-dom';
import 'normalize.css';
import App from './App';
import * as serviceWorker from './serviceWorker';

ReactDOM.render(<App />, document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

console.log('process.env = ', process.env);

let cnzz_protocol = (("https:" === document.location.protocol) ? "https://" : "http://");

document.write(
  "<span style='display: none' id='cnzz_stat_icon_" + process.env.REACT_APP_CNZZ_ID + "'></span>" + 
  "<script " + 
      "src='" + cnzz_protocol + "s5.cnzz.com/z_stat.php?id=" + process.env.REACT_APP_CNZZ_ID + "&show=pic1' " + 
      "type='text/javascript'>" + 
  "</script>"
);