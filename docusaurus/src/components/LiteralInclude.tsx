import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import CodeBlock from "@theme/CodeBlock";
const LiteralInclude = ({
  file,
  title,
  start,
  end,
  language,
}: {
  file: string;
  title?: string;
  start: string;
  end: string;
  language: string;
}) => {
  const { siteConfig } = useDocusaurusContext();
  let codeString = siteConfig.themeConfig.fileContents[file];
  if (!codeString) {
    return "Code block not found";
  }
  if (start && end) {
    const startLine = codeString.indexOf(start);
    const endLine = codeString.indexOf(end);
    if (startLine === -1 || endLine === -1) {
      return "Unable to find start or end line in code block";
    }
    codeString = codeString.slice(startLine + start.length, endLine).trim();
  }
  return (
    <CodeBlock language={language} title={title || file} showLineNumbers>
      {codeString}
    </CodeBlock>
  );
};

export default LiteralInclude;
