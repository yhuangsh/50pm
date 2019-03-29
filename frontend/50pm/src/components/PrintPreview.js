import React from 'react';
import styled from 'styled-components';
import { 
  A4Portrait, 
  PageBreak
} from './Paper';

import EquationList from '../components/EquationList';

const Frame = styled.div`
  @media screen {
    min-width: 250mm;
    margin: 1ex .5em;
    padding: 3ex 0;
    /*border: 1px solid black;*/
  }
`;

const PrintPreview = (props) => {
  let equs = props.equs;
  let lastEqu = equs.pop();

  console.log('PrintPreview: equs = ', equs);
  console.log('PrintPreview: lastEqu = ', lastEqu);
  return (
    <Frame>
      {equs.map(x => (
        <React.Fragment key={x.id}>
          <A4Portrait>
            <EquationList equs={x.equs}/>
          </A4Portrait>
          <PageBreak/>
        </React.Fragment>
      ))}
      {(lastEqu && 
        <A4Portrait>
          <EquationList equs={lastEqu.equs}/>
        </A4Portrait>)}
    </Frame>
  );
}

export default PrintPreview;
