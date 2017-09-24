import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {accountGravatarUrl} from '../Account'
import {App, navigateTo} from '../../app'
import {Account, Boulder, prettyPrintSector} from '../../storage'

import {lightGrey, darkGrey, text, gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'
import {useTypeface, copy16} from '../../Materials/Typefaces'

export interface BoulderCardProps {
    app: App
    boulderE: Avers.Editable<Boulder>
}

export class BoulderCard extends React.Component<BoulderCardProps> {
    onClick = () => {
        navigateTo('/boulder/' + this.props.boulderE.objectId)
    }

    render() {
        const {app, boulderE} = this.props
        const {objectId, content} = boulderE
        const {grade, gradeNr, sector, setter} = content

        return (
            <Card onClick={this.onClick} grade={grade}>
                <Id>{gradeNr}</Id>
                <Meta>
                    <Sector>{prettyPrintSector(sector)}</Sector>
                    <Setters>
                        {setter.map(setterId => <BoulderCardSetter app={app} setterId={setterId} />)}
                    </Setters>
                </Meta>
            </Card>
        )
    }
}

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
const BoulderCardSetter = ({app, setterId}: {app: App, setterId: string}) => (
    <Setter src={Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => accountGravatarUrl(account.email)).get(placeholderImageSrc)} />
)



// ----------------------------------------------------------------------------

const Id = styled.div`
    width: 32px;
    height: 32px;
    border-radius: 100%;
    border: 2px solid transparent;
    display: flex;
    justify-content: center;
    align-items: center;
    font-size: 22px;
    line-height: 1;
    box-shadow: 0 0 4px rgba(99, 85, 25, .5);
    margin-right: 12px;
    font-family: 'Advent Pro';
    transition: box-shadow .2s;
    flex: 0 0 32px;

    @media (min-width: 480px) {
        width: 48px;
        height: 48px;
        flex: 0 0 48px;
        font-size: 36px;
    }
`

const Card: any = styled.div`
    display: flex;
    flex-direction: row;
    cursor: pointer;
    padding: 8px 16px;
    width: 100%;
    overflow: hidden;

    & ${Id} {
        background-color: ${(props: any) => gradeBackgroundColor(props.grade)};
        border-color: ${(props: any) => gradeBorderColor(props.grade)};
        color: ${(props: any) => gradeColor(props.grade)};
        transition: transform .2s;
    }

    &:hover ${Id} {
        box-shadow: 0 0 8px rgba(99, 85, 25, .5);
        transform: scale(1.08);
    }

    @media (min-width: 480px) {
        padding: 16px 24px;
        width: calc(100% / 2);
    }

    @media (min-width: 720px) {
        padding: 16px 24px;
        width: calc(100% / 3);
    }

    @media (min-width: 960px) {
        width: calc(100% / 4);
    }

    @media (min-width: 1200px) {
        width: calc(100% / 5);
    }

`

const Meta = styled.div`
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;
    flex: 1;

    @media (min-width: 480px) {
        flex-direction: column;
        align-items: flex-start;
        justify-content: center;
    }
`

const Sector = styled.div`
    ${useTypeface(copy16)}
    white-space: nowrap;
    line-height: 1;
    text-transform: uppercase;
`

const Setters = styled.div`
    display: flex;
    flex-direction: row;

    @media (min-width: 480px) {
        margin-top: 8px;
    }
`

const Setter = styled.img`
    width: 32px;
    height: 32px;
    margin: 0 0 0 4px;
    display: block;
    border-radius: 2px;

    @media (min-width: 480px) {
        width: 24px;
        height: 24px;
        margin: 0 4px 0 0;
    }
`
