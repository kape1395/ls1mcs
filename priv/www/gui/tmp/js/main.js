
function ls1mcs_init() {
    ls1mcs_immcmds_init();
    ls1mcs_telemetry_init();
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
    $("#immediate-commands").on("click", "a[href='#immcmd__list_refresh']", function () {
        ls1mcs_immcmds_show();
    });

    $("#immcmd-send-table").on("click", "a[href='#immcmd__ping']", function () {
        ls1mcs_send_immediate_command(
            {spec: "ping"}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__job_off']", function () {
        var jobid = parseInt($("#immcmd__job_jobid").val());
        ls1mcs_send_immediate_command(
            {spec: "job_period", args: [
                {name: "jobid",    value: jobid},
                {name: "interval", value: 0}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__job_set']", function () {
        var jobid = parseInt($("#immcmd__job_jobid").val());
        var interval = parseInt($("#immcmd__job_int").val());
        ls1mcs_send_immediate_command(
            {spec: "job_period", args: [
                {name: "jobid",    value: jobid},
                {name: "interval", value: interval}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__pwr_set']", function () {
        var mode = parseInt($("#immcmd__pwr_mode").val());
        ls1mcs_send_immediate_command({spec: "pwr_state", args: [{name: "mode", value: mode}]});
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__pwrnm_allow']", function () {
        ls1mcs_send_immediate_command({spec: "pwr_allow_nm", args: [{name: "allow", value: 1}]});
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__pwrnm_dis']", function () {
        ls1mcs_send_immediate_command({spec: "pwr_allow_nm", args: [{name: "allow", value: 0}]});
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

    $("#immcmd-send-table").on("click", "a[href='#immcmd__eps_reset']", function () {
        ls1mcs_send_immediate_command(
            {spec: "hrd_reset"}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__fmrep_on']", function () {
        var delay = parseInt($("#immcmd__fmrep_delay").val());
        var duration = parseInt($("#immcmd__fmrep_duration").val());
        ls1mcs_send_immediate_command(
            {spec: "start_fmrep", args: [
                {name: "delay",     value: delay},
                {name: "duration",  value: duration}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__scimode_term']", function () {
        ls1mcs_send_immediate_command(
            {spec: "term_sci_mode"}
        );
    });


    $("#immcmd-send-table").on("click", "a[href='#immcmd__photo_take']", function () {
        var resid = parseInt($("#immcmd__photo_resid").val());
        var delay = parseInt($("#immcmd__photo_delay").val());
        ls1mcs_send_immediate_command(
            {spec: "take_photo", args: [
                {name: "resid", value: resid},
                {name: "delay", value: delay}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__photo_dlnk']", function () {
        ls1mcs_send_immediate_command(
            {spec: "dlnk_photo"}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__phfrag']", function () {
        var blksz = parseInt($("#immcmd__phfrag_blksz").val());
        var from  = parseInt($("#immcmd__phfrag_from").val());
        var till  = parseInt($("#immcmd__phfrag_till").val());
        ls1mcs_send_immediate_command(
            {spec: "photo_data", args: [
                {name: "blksz", value: blksz},
                {name: "from",  value: from},
                {name: "till",  value: till}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__dlnk']", function () {
        var bufid = parseInt($("#immcmd__dlnk_bufid").val());
        var blksz = parseInt($("#immcmd__dlnk_blksz").val());
        var from  = parseInt($("#immcmd__dlnk_from").val());
        var till  = parseInt($("#immcmd__dlnk_till").val());
        ls1mcs_send_immediate_command(
            {spec: "downlink", args: [
                {name: "bufid", value: bufid},
                {name: "blksz", value: blksz},
                {name: "from",  value: from},
                {name: "till",  value: till}
            ]}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__arm_sd_format']", function () {
        ls1mcs_send_immediate_command(
            {spec: "sd_format"}
        );
    });

    $("#immcmd-send-table").on("click", "a[href='#immcmd__beacon_on']", function () {
        ls1mcs_send_immediate_command({spec: "beacon_st", args: [{name: "status", value: 1}]});
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__beacon_off']", function () {
        ls1mcs_send_immediate_command({spec: "beacon_st", args: [{name: "status", value: 0}]});
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__rttm_get']", function () {
        ls1mcs_send_immediate_command({spec: "runtime_tm"});
    });

    $("#immcmd-send-table").on("click", "a[href='#immcmd__he_restore']", function () {
        ls1mcs_send_immediate_command(
            {spec: "he_restore"}
        );
    });
    $("#immcmd-send-table").on("click", "a[href='#immcmd__he_tx_pwr']", function () {
        var level  = parseInt($("#immcmd__he_tx_pwr_level").val());
        ls1mcs_send_immediate_command(
            {spec: "he_tx_prw", args: [
                {name: "level",  value: level}
            ]}
        );
    });

    ls1mcs_immcmds_show();
}

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
        rows += "<td>" + c.spec;
        if (c._links.photo != undefined) {
            rows += " (<a href='" + c._links.photo.href + "' target='_blank'>photo</a>)"
        }
        rows += "</td>";
        rows += "<td>" + c.status + "</td>";
        rows += "</tr>";
    }
    $("#immcmd-list-table > tbody").html(rows);
}


// =============================================================================
//  Main tabs: "telemetry" tab
// =============================================================================

function ls1mcs_telemetry_init() {
    $("html").on("click", "a[href='#telemetry-refresh']", function () {
        ls1mcs_telemetry_load();
    });
}
function ls1mcs_telemetry_show() {
    ls1mcs_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#telemetry']").tab('show');
}

function ls1mcs_telemetry_load() {
    $.getJSON(api_url("telemetry/gs/latest"), function (data, textStatus, jqXHR) {
        ls1mcs_telemetry_render(data);
    });
}

function ls1mcs_telemetry_render(telemetry) {
    $("#telemetry-data-latest").html(JSON.stringify(telemetry, undefined, 4));
}
