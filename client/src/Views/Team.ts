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
import {SetterCard} from './Components/Setter';

import {Account} from '../storage';


function setterLoadingView(app: App, accountId: string) {
    return React.DOM.div
        ( { key: accountId, className: 'setter-card' }
        , accountId
        );
}


export function
teamView(app: App) {
    var setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
        let accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);

        return accountC.fmap((account) => {
            return SetterCard({ app: app, key: accountId, account: account, accountId: accountId });
        }).get(setterLoadingView(app, accountId));
    });

    return site
        ( app
        , navBar(app)
        , React.DOM.div
            ( { className: 'team' }
            , React.DOM.h1
                ( { }
                , 'Team'
                )
            , React.DOM.div
                ( { }
                , setters
                )
            )
        );
}
