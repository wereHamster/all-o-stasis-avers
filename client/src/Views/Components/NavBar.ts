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

var teamItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/team') }
    , React.DOM.i({ className: 'users icon' })
    , ''
    );

var statsItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/stats') }
    , React.DOM.i({ className: 'line chart icon' })
    , ''
    );


// spacer
var flexItem = React.DOM.div
    ( { className: 'flex-navbar-item' }
    );

// Account options
var signInItem = React.DOM.div
    ( { className: 'navbar-item', onClick: navigateToFn('/login') }
    , React.DOM.i({ className: 'sign in icon' })
    , 'Login'
    );

// FIXME: that should show a dropdown with signout and account preferences
function accountSettingsItem(app: App) {
    var accountId = app.data.session.objId;

    return React.DOM.div
        ( { className: 'navbar-item', onClick: navigateToFn('/account/' + accountId) }
        , React.DOM.i({ className: 'user icon' })
        , 'Account'
        );
}


export function
navBar(app: App) {
    if (app.data.session.objId) {
        return React.DOM.div
            ( { className: 'navbar' }
            //, sideBarItem
            , homeItem
            , statsItem
            , teamItem
            , flexItem
            , Boulder.createBoulderItem(app) //FIXME: if has permissions (role)
            , accountSettingsItem(app)
            );

    } else {
        return React.DOM.div
            ( { className: 'navbar' }
            //, sideBarItem
            , homeItem
            , statsItem
            , teamItem
            , flexItem
            , signInItem
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
