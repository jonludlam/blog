import React from 'react';
import Sidebar from '../Sidebar';
import './style.scss';

class PageTemplateDetails extends React.Component {
  render() {
    const page = this.props.data.markdownRemark;

    return (
      <div>
        <Sidebar {...this.props} />
        <div className="content">
          <div className="content__inner">
            <div className="page">
              <h1 className="page__title">{page.frontmatter.title} foo</h1>
              <div>
                evaluator results: 
              </div>

              <div className="page__body" dangerouslySetInnerHTML={{ __html: page.html }} />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default PageTemplateDetails;
