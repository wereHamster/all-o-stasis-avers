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
import {SetterCard} from './Components/SetterCard';

export function
teamView(app: App) {
    const setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
        const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);
        return accountC.fmap(account =>
            <SetterCard key={accountId} app={app} accountId={accountId} account={account} />
        ).get(<SetterCard key={accountId} app={app} accountId={accountId} account={undefined} />);
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
