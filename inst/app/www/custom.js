/* bayprior — custom JavaScript for the Shiny/shinydashboard app */

/* ── Sidebar collapse memory ─────────────────────────────────────────────────
   Remember collapsed/expanded state of sidebar across page refreshes.       */
$(document).ready(function () {

  // Restore sidebar state
  if (localStorage.getItem('bayprior_sidebar_collapsed') === 'true') {
    $('body').addClass('sidebar-collapse');
  }

  // Save state on toggle
  $(document).on('click', '.sidebar-toggle', function () {
    var collapsed = $('body').hasClass('sidebar-collapse');
    localStorage.setItem('bayprior_sidebar_collapsed', collapsed ? 'false' : 'true');
  });

  /* ── Active tab highlight ───────────────────────────────────────────────── */
  $(document).on('click', '.sidebar-menu a', function () {
    $('.sidebar-menu li').removeClass('active');
    $(this).closest('li').addClass('active');
  });

  /* ── Plotly responsive resize ───────────────────────────────────────────── */
  // After resize, re-apply dark mode patch if needed since Plotly redraws
  // reset inline SVG styles back to defaults.
  $(window).on('resize', function () {
    $('.js-plotly-plot').each(function () {
      Plotly.Plots.resize(this);
    });
    // Re-patch after resize completes (Plotly redraws reset inline styles)
    if (typeof patchPlotlyDark === 'function') {
      setTimeout(patchPlotlyDark, 300);
    }
  });

  /* ── Chip counter update helper (roulette module) ───────────────────────── */
  Shiny.addCustomMessageHandler('update_chip_count', function (msg) {
    $('#' + msg.id).text(msg.value);
  });

  /* ── Notification auto-dismiss after 4 s ────────────────────────────────── */
  $(document).on('DOMNodeInserted', '.shiny-notification', function () {
    var el = this;
    setTimeout(function () {
      $(el).fadeOut(400, function () { $(el).remove(); });
    }, 4000);
  });

});