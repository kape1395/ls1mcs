
function ls1mcs_init() {
    ls1mcs_immcmds_init();
    ls1mcs_schedcmd_init();
    ls1mcs_dlnkphoto_init();
    ls1mcs_cmdlog_init();
    ls1mcs_telemetry_init();
    ls1mcs_upload_init();
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

function ls1mcs_send_immediate_command(command) {
    $.ajax({
        type: "POST",
        url: api_url("command/immediate"),
        data: JSON.stringify(command),
        success: function () {ls1mcs_immcmds_load();},
        dataType: "json"
    });
}
function ls1mcs_refresh_lasttm() {
    $.getJSON(api_url("telemetry/gs/latest"), function (data, textStatus, jqXHR) {
        var tmid = data.id;
        var recv = data.recv;
        var eps = data.hk.eps;
        $("#immcmd__last_tm_recv")      .html("" + ls1mcs_format_time(recv) + " (" + tmid + ")");
        $("#immcmd__last_tm_pv1")       .html("" + eps.pv_1);
        $("#immcmd__last_tm_pv2")       .html("" + eps.pv_2);
        $("#immcmd__last_tm_pv3")       .html("" + eps.pv_3);
        $("#immcmd__last_tm_pc")        .html("" + eps.pc);
        $("#immcmd__last_tm_bv")        .html("" + eps.bv);
        $("#immcmd__last_tm_sc")        .html("" + eps.sc);
        $("#immcmd__last_tm_tempob")    .html("" + eps.temp_OB);
        $("#immcmd__last_tm_reset")     .html("" + eps.reset);
        $("#immcmd__last_tm_bootcount") .html("" + eps.bootcount);
        $("#immcmd__last_tm_ch50v1")    .html("" + eps.channel_status_50V1);
        $("#immcmd__last_tm_ch50v2")    .html("" + eps.channel_status_50V2);
        $("#immcmd__last_tm_ch50v3")    .html("" + eps.channel_status_50V3);
        $("#immcmd__last_tm_ch33v1")    .html("" + eps.channel_status_33V1);
        $("#immcmd__last_tm_ch33v2")    .html("" + eps.channel_status_33V2);
        $("#immcmd__last_tm_ch33v3")    .html("" + eps.channel_status_33V3);
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
    $("#immcmd-send-table").on("click", "a[href='#immcmd__arm_set_started']", function () {
        ls1mcs_send_immediate_command(
            {spec: "set_started"}
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


    $("#immediate-commands").on("click", "a[href='#immcmd__list_refresh']", function () {
        ls1mcs_immcmds_show();
    });


    $("#immcmd-last_tm").on("click", "a[href='#immcmd__last_tm_refresh']", function () {
        ls1mcs_refresh_lasttm();
    });
    setInterval(ls1mcs_refresh_lasttm, 2000);


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
        rows += "("
        for (var a = 0; c.args != null && a < c.args.length; a++) {
            if (a > 0) rows += ", ";
            rows += c.args[a].name + "=" + c.args[a].value;
        }
        rows += ")"
        if (c._links.photo != undefined) {
            rows += " [<a href='" + c._links.photo.href + "' target='_blank'>photo</a>]"
        }
        rows += "</td>";
        rows += "<td>" + c.status + "</td>";
        rows += "</tr>";
    }
    $("#immcmd-list-table > tbody").html(rows);
}

// =============================================================================
//  Main tabs: "scheduled command" tab
// =============================================================================

function ls1mcs_send_scheduled_command(command) {
    $.ajax({
        type: "POST",
        url: api_url("command/scheduled"),
        data: JSON.stringify(command),
        success: function () {ls1mcs_schedcmd_load();},
        dataType: "json"
    });
}

function ls1mcs_schedcmd_init() {
    $("html").on("click", "a[href='#scheduled-commands']", function () {
        ls1mcs_schedcmd_show();
    });
    $("#schedcmd-list").on("click", "a[href='#chedcmd-refresh']", function () {
        ls1mcs_schedcmd_show();
    });
    $("#schedcmd-list").on("click", "a[href='#schedcmd-issue']", function () {
        var from  = $("#schedcmd-from").val();
        var till  = $("#schedcmd-till").val();
        var retry = parseInt($("#schedcmd-retry").val());
        ls1mcs_send_scheduled_command(
            {spec: "shed_ping", args: [
                {name: "from",  value: from},
                {name: "till",  value: till},
                {name: "retry", value: retry}
            ]}
        );
    });
    $("#schedcmd-list").on("click", "a[href='#schedcmd-cancel']", function () {
        var cmdId = $(this).closest("tr").data("id");
        $.ajax({
            type: "PUT",
            url: api_url("command/scheduled/" + cmdId),
            data: JSON.stringify({status: "canceled"}),
            success: function () {ls1mcs_schedcmd_show();},
            dataType: "json"
        });
    });
}

function ls1mcs_schedcmd_show() {
    ls1mcs_schedcmd_load();
    ls1mcs_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#scheduled-commands']").tab('show');
}

function ls1mcs_schedcmd_load() {
    $.getJSON(api_url("command/scheduled"), function (data, textStatus, jqXHR) {
        ls1mcs_schedcmd_render(data);
    });
}

function ls1mcs_schedcmd_render(commands) {
    commands.sort(function (a, b) { return (a.issued == b.issued) ? 0 : (a.issued > b.issued ? -1 : 1); });
    var rows = "";
    var arg_by_name = function (name, args) {
        for (a = 0; a < args.length; a++) {
            if (args[a].name == name)
                return args[a].value;
        }
        return undefined;
    };
    for (var i = 0; i < commands.length && i < 13; i++) {
        var c = commands[i];
        rows += "<tr data-id='" + c.id + "'>";
        rows += "<td>" + c.id + "</td>";
        rows += "<td>" + c.spec + "</td>";
        rows += "<td>" + arg_by_name("from", c.args) + "</td>";
        rows += "<td>" + arg_by_name("till", c.args) + "</td>";
        rows += "<td>" + arg_by_name("retry", c.args) + "</td>";
        rows += "<td>" + c.status + "</td>";
        if (c.status == "issued") {
            rows += "<td><a href='#schedcmd-cancel'>Cancel</a></td>";
        } else {
            rows += "<td>&nbsp;</td>";
        }
        rows += "</tr>";
    }
    $("#schedcmd-list > tbody").html(rows);
}


// =============================================================================
//  Main tabs: "Downlink photo" tab
// =============================================================================

function ls1mcs_dlnkphoto_download(cmdId, from, till) {
    $.ajax({
        type: "POST",
        url: api_url("command/usr/" + cmdId + "/photo/download"),
        data: JSON.stringify({from: from, till: till}),
        success: function () {},
        dataType: "json"
    });
}

function ls1mcs_dlnkphoto_init() {
    $("html").on("click", "a[href='#downlink-photo']", function () {
        ls1mcs_dlnkphoto_show();
    });
    $("#dlnk_photo-list").on("click", "a[href='#dlnk_photo-refresh']", function () {
        ls1mcs_dlnkphoto_show();
    });
    $("#dlnk_photo-list").on("click", "a[href='#dlnk_photo-set_range']", function () {
        var range = $(this).data("range");
        var cmdId = $(this).closest("tr").data("id");
        $("#dlnk_photo-list > tbody > tr[data-id='" + cmdId + "'] .dlnk_photo-download_range").val(range);
    });
    $("#dlnk_photo-list").on("click", "a[href='#dlnk_photo-download']", function () {
        var cmdId = $(this).closest("tr").data("id");
        var range = $("#dlnk_photo-list > tbody > tr[data-id='" + cmdId + "'] .dlnk_photo-download_range").val();
        var rangeElems = range.split("-");
        var from = rangeElems[0];
        var till = rangeElems[1];
        ls1mcs_dlnkphoto_download(cmdId, from, till);
    });
    $("#dlnk_photo-list").on("click", "a[href='#dlnk_photo-cancel']", function () {
        var cmdId = $(this).closest("tr").data("id");
        console.log("aaa");
        if (window.confirm("Cancel photo download process " + cmdId + "?")) {
            $.ajax({
                type: "POST",
                url: api_url("command/usr/" + cmdId + "/photo/cancel"),
                data: JSON.stringify({reason: "user request"}),
                success: function () {ls1mcs_dlnkphoto_show();},
                dataType: "json"
            });
        }
    });
}

function ls1mcs_dlnkphoto_show() {
    ls1mcs_dlnkphoto_load();
    ls1mcs_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#downlink-photo']").tab('show');
}

function ls1mcs_dlnkphoto_load() {
    $.getJSON(api_url("command/dlnk_photo"), function (data, textStatus, jqXHR) {
        ls1mcs_dlnkphoto_render(data);
    });
}

function ls1mcs_dlnkphoto_load_meta(cmdId) {
    $.getJSON(api_url("command/usr/" + cmdId + "/photo?t=meta"), function (data, textStatus, jqXHR) {
        ls1mcs_dlnkphoto_render_meta(cmdId, data);
    });
}

function ls1mcs_dlnkphoto_render(commands) {
    commands.sort(function (a, b) { return (a.issued == b.issued) ? 0 : (a.issued > b.issued ? -1 : 1); });
    var rows = "";
    var arg_by_name = function (name, args) {
        for (a = 0; a < args.length; a++) {
            if (args[a].name == name)
                return args[a].value;
        }
        return undefined;
    };
    for (var i = 0; i < commands.length && i < 13; i++) {
        var c = commands[i];
        rows += "<tr data-id='" + c.id + "'><td>";
        rows += "<div><span>Id: " + c.id + "</span> [<a href='#dlnk_photo-cancel'>Cancel</a>]</div>";
        rows += "<div>"
        rows += "<input type='text' class='dlnk_photo-download_range' placeholder='0-314'> ";
        rows += "<a href='#dlnk_photo-download'>Download</a>";
        rows += "</div>";
        rows += "<div>Missing intervals: <span class='dlnk_photo-missing_count'>?</span><ul class='dlnk_photo-missing_list'></ul></div>";
        rows += "</td><td>";
        rows += "<img src='" + api_url("command/usr/" + c.id + "/photo") + "?dummy=" + new Date().getTime() + "'>";
        rows += "</td></tr>";
        ls1mcs_dlnkphoto_load_meta(c.id);
    }
    $("#dlnk_photo-list > tbody").html(rows);
}

function ls1mcs_dlnkphoto_render_meta(cmdId, data) {
    var count = data.length;
    var list = "";
    for (i = 0; i < count; i++) {
        var interval = data[i];
        var tillValue;
        var tillDesc;
        if (interval.till == null) {
            tillDesc = "?";
            tillValue = interval.from + 350;
        } else {
            tillDesc = interval.till;
            tillValue = interval.till;
        }
        list += "<li><a data-range='" + interval.from + "-" + tillValue + "' href='#dlnk_photo-set_range'>";
        list += "" + interval.from + "-" + tillDesc;
        list += "</a></li>";
    }
    $("#dlnk_photo-list > tbody > tr[data-id='" + cmdId + "'] .dlnk_photo-missing_count").html("" + count);
    $("#dlnk_photo-list > tbody > tr[data-id='" + cmdId + "'] .dlnk_photo-missing_list").html(list);
}


// =============================================================================
//  Main tabs: "command log" tab
// =============================================================================

function ls1mcs_cmdlog_init() {
    $("#command-log").on("click", "a[href='#command-log_refresh']", function () {
        ls1mcs_cmdlog_show();
    });
    $("#command-log").on("click", "a[href='#command-log_confirm']", function () {
        if (window.confirm("Confirm, that command was executed successfully?")) {
            var cmdId = $(this).closest("tr").data("id");
            $.ajax({
                type: "PUT",
                url: api_url("command/usr/" + cmdId),
                data: JSON.stringify({status: "confirmed"}),
                success: function () {ls1mcs_cmdlog_show();},
                dataType: "json"
            });
        }
    });
}

function ls1mcs_cmdlog_show() {
    ls1mcs_cmdlog_load();
    ls1mcs_pages_show_main();
    $("#main-tabs > ul > li > a[href = '#command-log']").tab('show');
}

function ls1mcs_cmdlog_load() {
    $.getJSON(api_url("command/usr"), function (data, textStatus, jqXHR) {
        ls1mcs_cmdlog_render(data);
    });
}

function ls1mcs_cmdlog_render(commands) {
    commands.sort(function (a, b) { return (a.issued == b.issued) ? 0 : (a.issued > b.issued ? -1 : 1); });
    var rows = "";
    for (var i = 0; i < commands.length && i < 1000; i++) {
        var c = commands[i];
        rows += "<tr data-id='" + c.id + "'>";
        rows += "<td>" + c.id + "</td>";
        rows += "<td>" + c.spec + "</td>";
        if (c.args == null) {
            rows += "<td>&nbsp;</td>";
        } else {
            rows += "<td>";
            for (var a = 0; a < c.args.length; a++) {
                if (a > 0) rows += " ";
                rows += c.args[a].name + "=" + c.args[a].value;
            }
            rows += "</td>";
        }
        rows += "<td>" + c.issued + "</td>";
        rows += "<td>" + c.status + "</td>";
        rows += "<td>";
        if (c.status != "confirmed") {
            rows += "<a href='#command-log_confirm'>Confirm</a>";
        }
        rows += "&nbsp;</td>";
        rows += "</tr>";
    }
    $("#command-log_table > tbody").html(rows);
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


// =============================================================================
//  Main tabs: "upload" tab
// =============================================================================

function ls1mcs_upload_init() {
    $("#upload-form").on("click", "#upload-decode_tm", function () {
        $("#upload-form").attr("action", api_url("telemetry/ham?m=preview")).submit();
    });
    $("#upload-form").on("click", "#upload-import_frames", function () {
        $("#upload-form").attr("action", api_url("ls1p_frame?m=import")).submit();
    });
}

// =============================================================================
function ls1mcs_format_time(time) {
    var split = time.replace("T", "&nbsp;").split(".");
    if (split.length > 0) {
        return split[0];
    } else {
        return time;
    }
}
