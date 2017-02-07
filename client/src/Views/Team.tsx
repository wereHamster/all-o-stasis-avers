/*
module Team
( teamView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import Computation from 'computation';
import {App} from '../app';

import {Site} from './Components/Site';
import {SetterCard} from './Components/Setter';

import {Account} from '../storage';


function setterLoadingView(app: App, accountId: string) {
    return (
      <div key={accountId} className='setter-card'>
        {accountId}
      </div>
    );
}


export function
teamView(app: App) {
    var setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
        let accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);

        return accountC.fmap((account) => {
            return SetterCard({ app: app, key: accountId, account: account, accountId: accountId }) as JSX.Element;
        }).get(setterLoadingView(app, accountId));
    });

    return (
      <Site app={app}>
        <div className='team'>
          <h1>Team</h1>
          <div>
            {setters}
          </div>
        </div>
      </Site>
    );
}
