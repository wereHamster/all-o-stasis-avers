/*
module SideBar
( sideBar
) where
*/


import * as Avers from '../../lib/avers';
import {App, navigateTo, navigateToFn} from '../../app';
import * as Boulder from '../Boulder';


export function
sideBar(app: App) {

    var sideBar = React.DOM.div
        ( { className: 'ui inverted labeled icon left inline vertical sidebar menu' }
        , React.DOM.a({ className: 'item' }
            , React.DOM.i({ className: 'search icon' })
            , 'Search'
            )
        );

    return sideBar;
}
