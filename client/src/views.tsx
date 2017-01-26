/*
module Views
( loadingView
, notFoundView
) where
*/


import {App} from './app';

import {Site} from './Views/Components/Site';

export function loadingView(app: App) {
    return (
      <div className="login">
        <div className="logo">
          Loading...
        </div>
      </div>
    );
}

export function notFoundView(app: App) {
    return (
      <Site app={app}>
        <div className="login">
          <div className="logo">
            Ooops..
          </div>
          <div className="about">
            Not Found <a href='/'>back</a>
          </div>
        </div>
      </Site>
    );
}
