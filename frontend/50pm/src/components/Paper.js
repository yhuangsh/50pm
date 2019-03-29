import React from 'react';
import styled from 'styled-components';

const PageBreak = styled.div`
  @media screen {
    margin: 2ex 0;
  }
  
  @media print {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
    page-break-after: always;
  }
`;

const StyledA4 = styled.div`
  overflow: hidden;
  margin: 0 auto;

  @media screen {
    box-shadow: 0 .5mm 2mm rgba(0,0,0,.3);
  }
`;

const StyledA4Portrait = styled(StyledA4)`
  width: 210mm;
  height: calc(.90*297mm);

  @media screen {
    height: 297mm;  
  }
`;

const StyledA4Landscape = styled(StyledA4)`
  width: 297mm;
  height: 210mm;
`;

const A4Portrait = props => ( <StyledA4Portrait {...props}/> );
const A4Landscape = props => ( <StyledA4Landscape {...props}/> );

export { 
  PageBreak,
  A4Portrait,
  A4Landscape,
};