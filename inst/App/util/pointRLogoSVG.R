pointRLogoSVG<-function(){HTML("
                          <svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:ev='http://www.w3.org/2001/xml-events' width='600' height='200'>
                            <filter id='genid312'>
                            <feGaussianBlur stdDeviation='5' in='SourceAlpha' result='genid309'/>
                            <feSpecularLighting surfaceScale='6' specularConstant='1' specularExponent='30' lighting-color='white' in='genid309' result='genid310'>
                            <fePointLight x='40' y='-30' z='200'/>
                            </feSpecularLighting>
                            <feComposite operator='in' in='genid310' in2='SourceAlpha' result='genid311'/>
                            <feMerge>
                            <feMergeNode in='SourceGraphic'/>
                            <feMergeNode in='genid311'/>
                            </feMerge>
                            </filter>
                            <g filter='url(#genid312)'>
                            <text fill='#0000AA' font-size='100' stroke='black' font-family='san serif' stroke-width='3' text-anchor='middle' dominant-baseline='central' x='240' y='100'>pointR</text>
                            <circle r='66.6666666666667' stroke-width='10' stroke='#100080' fill='none' cx='330' cy='100'/>
                            </g>
                            </svg>
                            "
)}