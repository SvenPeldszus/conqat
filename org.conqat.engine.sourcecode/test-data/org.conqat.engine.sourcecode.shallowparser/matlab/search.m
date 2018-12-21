% search.m from https://github.com/quandl/Matlab/blob/master/%2BQuandl/search.m
function output = search(query, varargin)

    % Parse input.
    p = inputParser;
    p.addRequired('query');
    p.addOptional('page',1);
    p.addOptional('source','');
    p.addOptional('silent',false);
    p.addOptional('results',3);
    p.addOptional('authcode',Quandl.auth());
    p.parse(query,varargin{:})
    params = containers.Map();
    params('page') = int2str(p.Results.page);
    source = p.Results.source;
    if not(strcmp(source, ''))
        params('source_code') = source;
    end
    silent = p.Results.silent;
    results = p.Results.results;
    authcode = p.Results.authcode;
    if size(authcode) > 0
        params('auth_token') = authcode;
    end
    params('query') = regexprep(query, ' ', '+');
    path = 'datasets.xml';
    response = Quandl.api(path, 'params', params);
    datasets = response.getDocumentElement;
    docs = datasets.getChildNodes;
    output = docs.item(7).getChildNodes;
    for i = 0:(output.getLength-1)
        if rem(i,2) == 1 && not(silent) && i < results*2
            node = output.item(i).item(1);
            while ~isempty(node)
                if strcmp(node.getNodeName, 'name')
                    fprintf('name: %s\n',char(node.getTextContent))
                end
                if strcmp(node.getNodeName, 'source-code')
                    source_code = char(node.getTextContent);
                end
                if strcmp(node.getNodeName, 'code')
                    fprintf('code: %s/%s\n', source_code, char(node.getTextContent))
                end
                if ismember(char(node.getNodeName), {'frequency', 'description'})
                    fprintf('%s: %s\n',char(node.getNodeName), char(node.getTextContent))
                end
                if strcmp(node.getNodeName, 'column-names')
                    fprintf('columns: ')
                    columns = node.getChildNodes;
                    for j = 1:(columns.getLength-1)
                        if rem(j,2) == 1
                            column = columns.item(j);
                            fprintf('%s', char(column.getTextContent))
                            if j < columns.getLength-2
                                fprintf('|')
                            end
                        end
                    end
                    fprintf('\n')
                end
                node = node.getNextSibling;
            end
            fprintf('\n')
        end
    end
end
