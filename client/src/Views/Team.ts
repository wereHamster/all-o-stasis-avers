/*
module Team
( teamView
) where
*/


import * as Avers from 'avers';
import Computation from 'computation';
import {App} from '../app';

import {navBar} from './Components/NavBar';
import {site} from './Components/Site';


export function
teamView(app: App) {

    var team = "";

    return site
        ( app
        , navBar(app)
        , React.DOM.div
            ( { className: 'login' }
            , React.DOM.div
                ( { className: 'logo' }
                , 'Team'
                )
            , React.DOM.p
                ( { className: 'about' }
                , 'All the setters..'
                )
            )
        , React.DOM.div({ className: 'team' }, team)
        );
}
