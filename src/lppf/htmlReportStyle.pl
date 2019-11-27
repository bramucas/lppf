html_report_template(HtmlReportTemplate) :-
    open('reportTemplate/reportTemplate.html', read, FileStream),
    read_stream_to_codes(FileStream, ReportTemplate),
    atom_codes(HtmlReportTemplate, ReportTemplate).

html_report_row_template(HtmlReportRowTemplate) :-
    open('reportTemplate/reportRowTemplate.html', read, FileStream),
    read_stream_to_codes(FileStream, RowTemplate),
    atom_codes(HtmlReportRowTemplate, RowTemplate).    