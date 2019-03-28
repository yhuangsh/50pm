
import React from 'react';
import styled from 'styled-components';

import * as g from '../lib/g';

// Constants

// Component Stylings

const Frame = styled.div`
  /* self */
  width: ${g.EQU_WIDTH};

  font-family: 'Fira Code', monospace;
  font-size: 1.2em;
  font-weight: 100;

  text-align: center;

  /* children */
  display: flex;
  flex-flow: row nowrap;
  align-content: center;  
`;

const Number = styled.div`
  flex: 2;
`;

const Operator = styled.div`
  flex: 1;
`;

const Unknown = styled.div`
  flex: 2;
  border-bottom: .5px solid black;
`;

// Component Compositions

const NumberOrUnknown = (props) => {
  if (props.n !== null)
    return (<Number>{props.n}</Number>);
  else
    return (<Unknown>{props.n}</Unknown>);
}

const Equation = (props) => {
  let [n1, opCode, n2, n3] = props.equ;
  let op = getOp(opCode);
 
  //console.log("equ =", props.equ);
  //console.log("op = ", op);
  return (
    <Frame>
      <NumberOrUnknown n={n1}/> 
      <Operator>{op}</Operator>
      <NumberOrUnknown n={n2}/> 
      <Operator> = </Operator>
      <NumberOrUnknown n={n3}/> 
    </Frame>
  );
}

export default Equation;

// Untility Functions

function getOp(n) {
    switch (n) {
        case 0: return "+";
        case 1: return "-";
        case 2: return "x";
        case 3: return "/";
        default: 
            console.log("ERROR: wrong opcode");
            return "?";
    }
}