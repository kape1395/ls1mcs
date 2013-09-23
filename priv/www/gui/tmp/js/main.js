
function ls1mcs_init() {
    ls1mcs_immcmds_init();
}

function api_url(resource) {
    return "/ls1mcs/api/" + resource;
}


//=============================================================================
//  Master pages
//=============================================================================

function ls1mcs_pages_show_main() {
    $("#pages > div").hide();
    $("#main-tabs").show();
}


// =============================================================================
//  Main tabs: "immediate command" tab
// =============================================================================

function ls1mcs_send_immediate_command(command)
{
    $.ajax({
        type: "POST",
        url: api_url("command/immediate"),
        data: JSON.stringify(command),
        success: function () {ls1mcs_immcmds_load();},
        dataType: "json"
    });
}
function ls1mcs_immcmds_init() {
    $("html").on("click", "a[href='#immediate-commands']", function () {
        ls1mcs_immcmds_show();
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__ping']", function () {
        ls1mcs_send_immediate_command(
            {spec: "ping"}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__tmattbr_off']", function () {
        ls1mcs_send_immediate_command(
            {spec: "job_period", args: [
                {name: "jobid",    value: 0},
                {name: "interval", value: 0}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__tmattbr_set']", function () {
        var interval = parseInt($("#immcmd__tmattbr_int").val());
        ls1mcs_send_immediate_command(
            {spec: "job_period", args: [
                {name: "jobid",    value: 0},
                {name: "interval", value: interval}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__epsch_on']", function () {
        var channel = parseInt($("#immcmd__epsch_channel").val());
        ls1mcs_send_immediate_command(
            {spec: "eps_ch_status", args: [
                {name: "channel",   value: channel},
                {name: "status",    value: 1}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__epsch_off']", function () {
        var channel = parseInt($("#immcmd__epsch_channel").val());
        ls1mcs_send_immediate_command(
            {spec: "eps_ch_status", args: [
                {name: "channel",   value: channel},
                {name: "status",    value: 0}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__fmrep_onoff']", function () {
        var delay = parseInt($("#immcmd__fmrep_delay").val());
        var duration = parseInt($("#immcmd__fmrep_duration").val());
        ls1mcs_send_immediate_command(
            {spec: "eps_ch_onoff", args: [
                {name: "channel",   value: 2},
                {name: "delay",     value: delay},
                {name: "duration",  value: duration}
            ]}
        );
    });

    ls1mcs_immcmds_show();
}


// -----------------------------------------------------------------------------
//  Model: list
//


function ls1mcs_immcmds_show() {
    ls1mcs_immcmds_load();
    ls1mcs_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#immediate-commands']").tab('show');
}

function ls1mcs_immcmds_load() {
    $.getJSON(api_url("command/usr"), function (data, textStatus, jqXHR) {
        ls1mcs_immcmds_render(data);
    });
}

function ls1mcs_immcmds_render(commands) {
    commands.sort(function (a, b) { return (a.issued == b.issued) ? 0 : (a.issued > b.issued ? -1 : 1); });
    var rows = "";
    for (var i = 0; i < commands.length && i < 13; i++) {
        var c = commands[i];
        rows += "<tr>";
        rows += "<td>" + c.id + "</td>";
        rows += "<td>" + c.spec + "</td>";
        rows += "<td>" + c.status + "</td>";
        rows += "</tr>";
    }
    $("#immcmd-list-table > tbody").html(rows);
}



