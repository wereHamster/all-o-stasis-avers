import * as React from 'react';

import * as Avers from 'avers';
import {App, navigateToFn} from '../../app';

import {Account} from '../../storage';
import {accountGravatarUrl} from '../Account';

export interface CardProps {
    account: Account;
    accountId: string;
    app: App;
}

function setterInfo(app: App, accountId: string) {
    return Avers.lookupContent<Account>(app.data.aversH, accountId).fmap(account => {

        var name = accountId;
        if (account.name != "")
            name = account.name;

        return (
          <div onClick={navigateToFn('/account/' + accountId)}>
            <img className="avatar" src={accountGravatarUrl(account.email)}/> 
            <div>{name}</div>
          </div>
        );
    }).get(undefined);
}

enum CardBody
    { Team
    }

interface CardState {
    cardBody: CardBody;
}

class CardSpec extends React.Component<CardProps, CardState> {

    private app : App;

    initialState(props: CardProps): CardState {
        return { cardBody: CardBody.Team };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        return (
          <div className='setter-card'>
            {setterInfo(this.props.app, this.props.accountId)}
          </div>
        );
    }
}

export var SetterCard = React.createFactory(CardSpec);
