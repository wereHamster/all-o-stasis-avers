/*
module DropDownInput
( DropDownInput
) where
*/

import * as React from 'react';
import * as ReactDOM from 'react-dom';

export interface DropdownInputProps {
    object  : any;
    field   : string;
    options : string[];
}

interface DropdownInputState {
    selectedValue : string;
}

function asString(value) {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}

class DropdownInputSpec extends React.Component<DropdownInputProps, DropdownInputState> {

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    initialState(props) {
        const selectedValue = props.object[props.field];
        return { selectedValue : asString(selectedValue) };
    }

    onChange = (e: React.FormEvent<any>) => {
        const value = (e.target as HTMLSelectElement).value;

        this.setState({ selectedValue: value });
        this.props.object[this.props.field] = value;
    };

    render() {
        function onClick(e) {
            e.stopPropagation();
        }

        const options = this.props.options.map( (entry, index) => {
            return (
                <option value={entry} key={index}>
                    {entry}
                </option>
            );
        });

        return (
            <select name={this.props.field} onClick={onClick}
                    onChange={this.onChange} defaultValue={this.state.selectedValue}>
              {options}
            </select>
        );
    }

    componentWillReceiveProps(props) {
        if (ReactDOM.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export const DropDownInput = React.createFactory(DropdownInputSpec);
