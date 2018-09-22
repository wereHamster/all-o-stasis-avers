import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'
import {Md5} from 'ts-md5/dist/md5'

import {role} from '../actions'
import {App} from '../app'
import {Account, roles, setterMonthlyStats} from '../storage'

import {useTypeface, heading28, copy16} from '../Materials/Typefaces'

import {DropDownInput} from './Components/DropdownInput'
import {Site} from './Components/Site'

export function
accountGravatarUrl(email: string) {
    return 'http://www.gravatar.com/avatar/' + Md5.hashStr(email)
}

// only admins can edit all accounts with the exception of users
// changing their own accounts
export const accountView = (accountId: string) => (app: App) => {
    return Avers.lookupEditable<Account>(app.data.aversH, accountId).fmap(accountE => {
        const canEdit = (role(app) === 'admin' || accountId === app.data.session.objId)
        return (
          <Site app={app}>
              { canEdit ? AccountView({ app, accountE }) : accountRep(accountE) }
          </Site>
        )
    }).get(<Site app={app} />)
}


// ----------------------------------------------------------------------------

// A simple account representation for non-owned and non-admin views.
function
accountRep(account: Avers.Editable<Account>): JSX.Element {
    return (
      <Avatar>
        <img src={accountGravatarUrl(account.content.email)}/>
        <Name>{account.content.name}</Name>
        <p className='about'>
          {account.content.role}
        </p>
      </Avatar>
    )
}


// Render all fields as editables.
export interface AccountViewProps {
    app: App
    accountE: Avers.Editable<Account>
}

class AccountSpec extends React.Component<AccountViewProps, {}> {
    changeAccountName = (e: React.FormEvent<any>) => {
        const value = (e.target as HTMLInputElement).value
        this.props.accountE.content.name = value
    }

    render() {
        const {accountE} = this.props

        return (
          <Root>
            {this.accountHeader(accountE)}
            {this.accountDetailsEditor(accountE)}
          </Root>
        )
    }

    accountHeader(accountE: Avers.Editable<Account>): JSX.Element {
        return (
          <Avatar>
            <img src={accountGravatarUrl(accountE.content.email)}/>
            <Name>{accountE.content.name}</Name>
            <Email>{accountE.content.email}</Email>
          </Avatar>
        )
    }

    accountAdminFields(accountE: Avers.Editable<Account>): JSX.Element {
        const account = accountE.content
        return (
          <div className='form-row'>
            <div className='label'>Role</div>
            <div className='content'>
              <DropDownInput object={account} field='role' options={roles()}></DropDownInput>
            </div>
          </div>
        )
    }

    accountDetailsEditor(accountE: Avers.Editable<Account>): JSX.Element {
        const account = accountE.content

        function onClick(e) {
            e.stopPropagation()
        }

        return (
          <div className='account-detail-editor'>
            <div className='form'>
              <div className='form-row'>
                <div className='label'>Name</div>
                <div className='content'>
                  <input className='wide' type='text' value={account.name}
                         onChange={this.changeAccountName} onClick={onClick}></input>
                </div>
              </div>
              {role(this.props.app) === 'admin' && this.accountAdminFields(accountE)}
            </div>

            <div>
                <h2>Stats for 09.2017</h2>
                <div>
                    {Avers.staticValue(this.props.app.data.aversH, setterMonthlyStats(this.props.app.data.aversH, accountE.objectId, 2017, 9)).fmap(sms => (
                        <div>{Object.keys(sms).map(grade => (
                            <div>{grade}: {sms[grade]}</div>
                        ))}</div>
                    )).get(<div/>)}
                </div>
            </div>
          </div>
        )
    }
}

const AccountView = React.createFactory(AccountSpec)



// ----------------------------------------------------------------------------

const Root = styled.div`
`

const Name = styled.div`
    ${useTypeface(heading28)}
    text-align: center;
    margin-top: 0.7rem;

`

const Email = styled.div`
    ${useTypeface(copy16)}
    text-align: center;
    margin-top: 0.2rem;
    margin-bottom: 0.2rem;

`

const Avatar = styled.div`
    display: flex;
    flex-direction: column;
    align-items: center;
    margin-top: 4rem;

    .about {
        max-width: 30rem;
        font-size: 1.2rem;
    }

    img {
        border: 1px solid #999;
        border-radius: 50%;
    }
`
