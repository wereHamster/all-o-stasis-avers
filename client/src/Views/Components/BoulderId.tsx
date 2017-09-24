import styled from 'styled-components'
import {lightGrey, darkGrey, text, gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'

export const BoulderId: any = styled.div`
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

    background-color: ${(props: any) => gradeBackgroundColor(props.grade)};
    border-color: ${(props: any) => gradeBorderColor(props.grade)};
    color: ${(props: any) => gradeColor(props.grade)};
    transition: transform .2s;

    @media (min-width: 480px) {
        width: 48px;
        height: 48px;
        flex: 0 0 48px;
        font-size: 36px;
    }
`
