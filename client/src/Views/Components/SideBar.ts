/*
module SideBar
( sideBar
) where
*/


import * as Avers from '../../lib/avers';
import {App, navigateTo, navigateToFn} from '../../app';
import * as Boulder from '../Boulder';

var sideBarItem = React.DOM.div
    ( { className: 'sidebar-item', onClick: navigateToFn('/') }
    , React.DOM.i({ className: 'sidebar icon' })
    , ''
    );

var homeItem = React.DOM.div
    ( { className: 'sidebar-item', onClick: navigateToFn('/') }
    , React.DOM.i({ className: 'home icon' })
    , ''
    );

// spacer
var flexItem = React.DOM.div
    ( { className: 'flex-sidebar-item' }
    );

var signInItem = React.DOM.div
    ( { className: 'sidebar-item', onClick: navigateToFn('/login') }
    , React.DOM.i({ className: 'sign in icon' })
    , 'Login'
    );


export function
sideBar(app: App) {

    var sideBar = React.DOM.div
        ( { className: 'ui inverted labeled icon left inline vertical sidebar menu' }
        , React.DOM.a({ className: 'item' }
            , React.DOM.i({ className: 'home icon' })
            , 'Home'
            )
        , React.DOM.a({ className: 'item' }
            , React.DOM.i({ className: 'block layout icon' })
            , 'Topics'
            )
        , React.DOM.a({ className: 'item' }
            , React.DOM.i({ className: 'smile icon' })
            , 'Friends'
            )
        , React.DOM.a({ className: 'item' }
            , React.DOM.i({ className: 'calendar icon' })
            , 'History'
            )
        );

    return sideBar

    if (app.data.session.objId) {
        return React.DOM.div
            ( { className: 'sidebar' }
            , sideBarItem
            , homeItem
            , flexItem
            );

    } else {
        return React.DOM.div
            ( { className: 'sidebar' }
            , sideBarItem
            , homeItem
            );
    }
}
