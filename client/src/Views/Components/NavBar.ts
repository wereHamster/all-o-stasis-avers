/*
module NavBar
( navBar
) where
*/


import * as Avers from '../../lib/avers';
import {App, navigateTo, navigateToFn} from '../../app';
import * as Boulder from '../Boulder';

var sideBarItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/') }
    , React.DOM.i({ className: 'sidebar icon' })
    , ''
    );

var homeItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/') }
    , React.DOM.i({ className: 'home icon' })
    , ''
    );

// spacer
var flexItem = React.DOM.div
    ( { className: 'flex-navbar-item' }
    );

// end session
// FIXME: that should show a dropdown with signout and preferences
function signOutItem(app: App) {
    return React.DOM.div
        ( { className: 'navbar-item', onClick: doSignOutF(app) }
        , React.DOM.i({ className: 'setting icon' })
        , 'Sign out'
        );
}

var signInItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/login') }
    , React.DOM.i({ className: 'sign in icon' })
    , 'Login'
    );


export function
navBar(app: App) {
    if (app.data.session.objId) {
        return React.DOM.div
            ( { className: 'navbar' }
            , sideBarItem
            , homeItem
            , flexItem
            , Boulder.createBoulderItem(app)
            , signOutItem(app)
            );

    } else {
        return React.DOM.div
            ( { className: 'navbar' }
            , sideBarItem
            , homeItem
            , signInItem
            , flexItem
            );
    }
}


function doSignOutF(app: App) {
    return e => {
        e.stopPropagation();
        Avers.signout(app.data.session).then(() => {
            navigateTo('/');
        });
    };
}
