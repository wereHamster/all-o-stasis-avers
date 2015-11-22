/// <reference path="./ext/react.d.ts" />

/*
module Views
( loadingView
, notFoundView
) where
*/


import {App} from './app';

import {site} from './Views/Components/Site';
import {navBar} from './Views/Components/NavBar';

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
    return site
        ( app
        , navBar(app)
        , <div className="login">
            <div className="logo">
              Ooops..
            </div>
            <div className="about">
              Not Found <a href='/'>back</a>
            </div>
          </div>
    );
}
