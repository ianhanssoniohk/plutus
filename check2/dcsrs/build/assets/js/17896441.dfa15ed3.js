"use strict";(self.webpackChunkdocusaurus=self.webpackChunkdocusaurus||[]).push([[8401],{6901:(e,t,n)=>{n.d(t,{A:()=>d});var s=n(3269),u=n(7639),a=n(2250),o=n(6540),i=n(4848);const c=e=>{let{file:t,title:n,start:s,end:c,language:d}=e;const{siteConfig:l}=(0,u.A)(),[r,f]=(0,o.useState)(!0),[g,b]=(0,o.useState)(""),[h,k]=(0,o.useState)("");return(0,o.useEffect)((()=>{let e=!0;return async function(){const n=await fetch(`/plutus/docs/code/${t}`),u=await n.text();if(e)if(f(!1),u||b("Code block not found"),s&&c){const e=u.indexOf(s),t=u.indexOf(c);-1===e||-1===t?b("Start and end lines not found in code block"):k(u.slice(e+s.length,t).trim())}else b("Start and end lines must be provided")}(),()=>{e=!1}}),[]),r?"Loading":g?"Error loading code block":(0,i.jsx)(a.A,{language:d,title:n||t,showLineNumbers:!0,children:h})},d={...s.A,LiteralInclude:c}}}]);