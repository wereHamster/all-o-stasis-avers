/*
module Team
( teamView
) where
*/

import * as Avers from 'avers';
import * as React from 'react';

import {App} from '../app';
import {Account} from '../storage';

import {Site} from './Components/Site';
import {SetterCard} from './Components/Setter';

function 
setterLoadingView(app: App, accountId: string) {
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
          <div className="logo">
            Team
          </div>
          <div className="members">
            {setters}
          </div>
        </div>
      </Site>
    );
}
