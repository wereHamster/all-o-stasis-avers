/*
module Login
( loginView
) where
*/

import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {useTypeface, h1, copy16, copy16Bold} from '../Materials/Typefaces'

import {App, navigateTo, navigateToFn} from '../app'
import {Site} from './Components/Site'

export const loginView = (accountId: string) => (app: App) => {
    return <LoginView app={app} />
}

function stopPropagation(e) {
    e.stopPropagation()
}

interface LoginState {
    email: string

    createPassportPromise: void | Promise<any>
    createPassportResponse: void | {passportId: string, securityCode: string}

    awaitPassportConfirmationPromise: void | Promise<any>
}

class LoginView extends React.Component<{ app: App }, LoginState> {

    state: LoginState = {
        email: '',

        createPassportPromise: undefined,
        createPassportResponse: undefined,

        awaitPassportConfirmationPromise: undefined,
    }

    onChangeEmail = (e) => {
        this.setState({
            email: e.target.value,

            createPassportPromise: undefined,
            createPassportResponse: undefined,

            awaitPassportConfirmationPromise: undefined,
        })
    }

    onReset = (e) => {
        e.preventDefault()
        this.setState({
            email: '',

            createPassportPromise: undefined,
            createPassportResponse: undefined,

            awaitPassportConfirmationPromise: undefined,
        })
    }

    doLogin = (e) => {
        const {app: {data: {aversH: {fetch, apiHost}, session}}} = this.props
        const {email} = this.state

        e.stopPropagation()

        const options = {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email })
        }

        const createPassportPromise = fetch(apiHost + '/login', options as any).then(res => {
            res.json().then(json => {
                const awaitPassportConfirmationPromise = fetch(apiHost + '/login/verify?passportId=' + json.passportId, {credentials: 'include'}).then(res => {
                    Avers.restoreSession(session)
                    navigateTo('/')
                })
                this.setState({ createPassportPromise: undefined, createPassportResponse: json, awaitPassportConfirmationPromise})
            })
        })
        this.setState({ createPassportPromise })
    }

    render() {
        const { app } = this.props
        const { email, createPassportPromise, createPassportResponse, awaitPassportConfirmationPromise } = this.state

        if (!awaitPassportConfirmationPromise) {
            return (
                <Site app={app}>
                    <Container>
                        <Form
                            email={email}
                            onChangeEmail={this.onChangeEmail}
                            doLogin={this.doLogin}
                            isSubmitting={createPassportPromise !== undefined}
                        />
                    </Container>
                </Site>
            )
        } else if (createPassportResponse && awaitPassportConfirmationPromise) {
            return (
                <Site app={app}>
                    <Container>
                        <AwaitingConfirmation
                            email={email}
                            onReset={this.onReset}
                            securityCode={createPassportResponse.securityCode}
                        />
                    </Container>
                </Site>
            )
        } else {
            return <div>IMPOSSIBLE</div>
        }
    }
}

const Container = ({children}) => (
    <div style={{flex: 1, display: 'flex', flexDirection: 'column', justifyContent: 'center', alignItems: 'center'}}>
        {children}
    </div>
)

export const Form = ({email, onChangeEmail, doLogin, isSubmitting}) => (
    <form style={{textAlign: 'center', maxWidth: 528}}>
        <H1>Authenticate</H1>
        <P>To sign up or log in, fill in your email address below:</P>
        <Input
            type='text'
            placeholder='you@domain.com'
            value={email}
            onChange={onChangeEmail}
            disabled={isSubmitting}
        />
        <Button onClick={doLogin} disabled={isSubmitting || email.length === 0}>GO</Button>
    </form>
)

export const AwaitingConfirmation = ({email, onReset, securityCode}) => (
    <div style={{textAlign: 'center', maxWidth: 528}}>
        <H1>Authenticating</H1>

        <P>
            We sent an email to <strong>{email}</strong> (<a href='#' onClick={onReset}>undo</a>).
        </P>

        <P>
            Please verify that the provided security code
            in the email matches the following text:
        </P>

        <SecurityCode>{securityCode}</SecurityCode>

        <P>Waiting for your confirmation…</P>
    </div>
)


const H1 = styled.h1`
${useTypeface(h1)};
`

const P = styled.p`
${useTypeface(copy16)};
`

const Input = styled.input`
${useTypeface(copy16)};
width: 100%;
text-align: center;

height: 40px;
line-height: 40px;
`

const Button = styled.button`
${useTypeface(copy16Bold)}
width: 100%;
border: none;
border-radius: 0;
background-color: rgb(246, 246, 246);
outline: none;

height: 40px;
margin-top: 10px;

transition: all .16s;

&:not(:disabled) {
    cursor: pointer;
}
&:not(:disabled):hover {
    background-color: rgb(230, 230, 230);
}
`

const SecurityCode = styled.div`
${useTypeface(copy16Bold)}
background-color: rgb(246, 246, 246);
font-weight: bold;
margin: 40px auto;
height: 40px;
display: flex;
align-items: center;
justify-content: center;
`
