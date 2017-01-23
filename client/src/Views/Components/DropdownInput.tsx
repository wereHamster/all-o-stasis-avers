/*
module DropDownInput
( DropDownInput
) where
*/


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

    initialState(props) {
        var selectedValue = props.object[props.field];
        return { selectedValue : asString(selectedValue) };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    onChange = (e: __React.FormEvent) => {
        let value = (e.target as HTMLSelectElement).value;

        this.setState({ selectedValue: value });
        this.props.object[this.props.field] = value;
    };

    render() {
        function onClick(e) {
            e.stopPropagation();
        }

        let options = this.props.options.map( entry => {
            return (
              <option value={entry}>
                {entry}
              </option>
            );
        });

        return (
            <select name={this.props.field} onClick={onClick} onChange={this.onChange}>
              {options}
            </select>);
    }

    componentWillReceiveProps(props) {
        if (ReactDOM.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export var DropDownInput = React.createFactory(DropdownInputSpec);
