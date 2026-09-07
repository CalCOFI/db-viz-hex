/* CCFeedback: the in-app feedback modal (functions.R::modal_feedback).
 *
 * - captures the map straight off its WebGL canvas when the modal opens
 *   (the maps carry preserveDrawingBuffer; read inside a 'render' handler),
 * - lets you mark it up (pen / arrow / box, 3 colours),
 * - posts the note CLIENT-SIDE to the Apps Script endpoint (same contract as
 *   CalCOFI/explore) or opens a prefilled GitHub issue.
 *
 * Vendored as a real .js so the annotator doesn't fight R string escaping.
 */
(function () {
  'use strict';

  function payload(box) {
    var t = document.getElementById('fb_text');
    var e = document.getElementById('fb_email');
    var inc = document.getElementById('fb_include');
    var p = {
      app: 'db-viz-hex',
      url: location.href,
      release: box.dataset.release || '',
      viewport: window.innerWidth + '×' + window.innerHeight,
      theme: document.documentElement.getAttribute('data-bs-theme') || 'light',
      text: ((t && t.value) || '').trim(),
      email: ((e && e.value) || '').trim(),
      website: '',
      user_agent: navigator.userAgent
    };
    if (inc && inc.checked && box.__shot) {
      try {
        var c = box.__shot;
        // downscale wide shots -- the Apps Script caps the decoded PNG at 6 MB,
        // and a full 4K tab easily exceeds that
        var MAXW = 1440;
        if (c.width > MAXW) {
          var d = document.createElement('canvas');
          d.width = MAXW; d.height = Math.round(c.height * MAXW / c.width);
          d.getContext('2d').drawImage(c, 0, 0, d.width, d.height);
          c = d;
        }
        // PNG, not JPEG: cc_feedback_script()'s doPost only accepts
        // data:image/png;base64 -- a JPEG data URL is silently dropped
        p.image = c.toDataURL('image/png');
      } catch (err) {}
    }
    return p;
  }

  function showShot(box) {
    var slot = box.querySelector('.cc-fb-shot-img');
    if (!slot || !box.__shot) return;
    var img = new Image();
    img.alt = 'captured view';
    img.src = box.__shot.toDataURL('image/jpeg', 0.85);
    slot.innerHTML = ''; slot.appendChild(img);
    var shot = box.querySelector('.cc-fb-shot');
    if (shot) shot.classList.add('has-shot');
  }

  var PH =
    '<div class="cc-fb-shot-ph">' +
      '<button type="button" class="cc-fb-capture">' +
        '<svg viewBox="0 0 16 16" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.6">' +
        '<rect x="2" y="4" width="12" height="9" rx="1.5"/><circle cx="8" cy="8.5" r="2.4"/>' +
        '<path d="M6 4l1-1.5h2L10 4"/></svg> Capture screenshot</button>' +
      '<p>You&rsquo;ll be asked to share this browser tab.</p>' +
    '</div>';

  // full-view screenshot via the browser's screen-capture API. Must be started
  // by a user gesture (the Capture button), so on modal-open we just show the
  // button. A DOM rasteriser (html2canvas / html-to-image) is NOT viable here --
  // html2canvas throws on color-mix(); html-to-image hangs inlining a dozen
  // stylesheets (incl. cross-origin calcofi.io theme.css) over thousands of nodes.
  function capture(box) {
    var slot = box.querySelector('.cc-fb-shot-img');
    if (!slot) return;
    var shot = box.querySelector('.cc-fb-shot');
    if (shot) shot.classList.remove('has-shot');
    box.__shot = null;
    slot.innerHTML = PH;
  }

  function grabScreen(box) {
    var slot = box.querySelector('.cc-fb-shot-img');
    var shot = box.querySelector('.cc-fb-shot');
    if (!navigator.mediaDevices || !navigator.mediaDevices.getDisplayMedia) {
      slot.innerHTML = '<div class="cc-fb-shot-wait">screen capture isn’t supported in this browser</div>';
      return;
    }
    slot.innerHTML = '<div class="cc-fb-shot-wait">pick this tab in the prompt…</div>';
    navigator.mediaDevices.getDisplayMedia({
      video: { displaySurface: 'browser' },
      preferCurrentTab: true,
      audio: false
    }).then(function (stream) {
      var video = document.createElement('video');
      video.srcObject = stream; video.muted = true;
      // hide the feedback modal + backdrop so the shot is the app view, not
      // this dialog; wait for the browser to repaint + the stream to catch up,
      // grab one frame, then restore.
      var modal = document.getElementById('shiny-modal');
      var bd = document.querySelector('.modal-backdrop');
      var mv = modal ? modal.style.visibility : '';
      var bv = bd ? bd.style.visibility : '';
      var finish = function () {
        if (modal) modal.style.visibility = 'hidden';
        if (bd) bd.style.visibility = 'hidden';
        setTimeout(function () {
          var w = video.videoWidth || 1280, h = video.videoHeight || 720;
          var out = document.createElement('canvas');
          out.width = w; out.height = h;
          try { out.getContext('2d').drawImage(video, 0, 0, w, h); } catch (e) {}
          if (modal) modal.style.visibility = mv;
          if (bd) bd.style.visibility = bv;
          stream.getTracks().forEach(function (t) { t.stop(); });
          video.srcObject = null;
          box.__shot = out;
          showShot(box);
        }, 450);
      };
      var p = video.play();
      if (p && p.then) p.then(function () { setTimeout(finish, 200); }).catch(function () { setTimeout(finish, 300); });
      else setTimeout(finish, 300);
    }).catch(function () {
      // user cancelled or denied -- back to the button
      if (shot) shot.classList.remove('has-shot');
      slot.innerHTML = PH;
    });
  }

  /* ---- annotator ------------------------------------------------------ */
  var COLORS = ['#ff2d78', '#ffcf33', '#33a0ff'];

  function annotate(box) {
    if (!box.__shot || box.querySelector('.cc-fb-anno')) return;
    var base = box.__shot;
    var shot = box.querySelector('.cc-fb-shot');

    var wrap = document.createElement('div');
    wrap.className = 'cc-fb-anno';
    wrap.innerHTML =
      '<div class="cc-fb-anno-stage"><canvas></canvas></div>' +
      '<div class="cc-fb-anno-bar">' +
        '<button type="button" data-tool="pen" class="on">Pen</button>' +
        '<button type="button" data-tool="arrow">Arrow</button>' +
        '<button type="button" data-tool="box">Box</button>' +
        '<button type="button" data-tool="text">Text</button>' +
        '<span class="cc-fb-anno-colors"></span>' +
        '<button type="button" data-act="undo">Undo</button>' +
        '<button type="button" data-act="clear">Clear</button>' +
        '<span class="cc-fb-anno-sp"></span>' +
        '<button type="button" data-act="cancel">Cancel</button>' +
        '<button type="button" data-act="done" class="prim">Done</button>' +
      '</div>';
    shot.appendChild(wrap);

    var cwrap = wrap.querySelector('.cc-fb-anno-colors');
    COLORS.forEach(function (c, i) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'cc-fb-anno-sw' + (i === 0 ? ' on' : '');
      b.style.background = c;
      b.dataset.color = c;
      cwrap.appendChild(b);
    });

    var cvs = wrap.querySelector('canvas');
    cvs.width = base.width; cvs.height = base.height;
    var ctx = cvs.getContext('2d');
    var strokes = [];
    var tool = 'pen', color = COLORS[0], drawing = null;

    function redraw() {
      ctx.clearRect(0, 0, cvs.width, cvs.height);
      ctx.drawImage(base, 0, 0);
      strokes.forEach(paint);
      if (drawing) paint(drawing);
    }
    function paint(s) {
      var p = s.pts;
      if (!p.length) return;
      ctx.strokeStyle = s.color; ctx.fillStyle = s.color;
      ctx.lineWidth = Math.max(2.5, cvs.width / 300);
      ctx.lineJoin = 'round'; ctx.lineCap = 'round';
      if (s.tool === 'pen') {
        ctx.beginPath(); ctx.moveTo(p[0].x, p[0].y);
        for (var i = 1; i < p.length; i++) ctx.lineTo(p[i].x, p[i].y);
        ctx.stroke();
      } else if (s.tool === 'box' && p.length > 1) {
        var a = p[0], b = p[p.length - 1];
        ctx.strokeRect(a.x, a.y, b.x - a.x, b.y - a.y);
      } else if (s.tool === 'arrow' && p.length > 1) {
        var a2 = p[0], b2 = p[p.length - 1];
        ctx.beginPath(); ctx.moveTo(a2.x, a2.y); ctx.lineTo(b2.x, b2.y); ctx.stroke();
        var ang = Math.atan2(b2.y - a2.y, b2.x - a2.x);
        var h = Math.max(12, cvs.width / 40);
        ctx.beginPath();
        ctx.moveTo(b2.x, b2.y);
        ctx.lineTo(b2.x - h * Math.cos(ang - 0.42), b2.y - h * Math.sin(ang - 0.42));
        ctx.lineTo(b2.x - h * Math.cos(ang + 0.42), b2.y - h * Math.sin(ang + 0.42));
        ctx.closePath(); ctx.fill();
      } else if (s.tool === 'text' && s.text) {
        var fz = s.size || Math.max(16, cvs.width / 36);
        ctx.font = '700 ' + fz + 'px system-ui, "Segoe UI", Roboto, sans-serif';
        ctx.textBaseline = 'top';
        ctx.lineJoin = 'round';
        ctx.strokeStyle = 'rgba(0, 0, 0, 0.55)';
        ctx.lineWidth = Math.max(2, fz / 7);
        ctx.strokeText(s.text, p[0].x, p[0].y);
        ctx.fillStyle = s.color;
        ctx.fillText(s.text, p[0].x, p[0].y);
      }
    }
    function pos(ev) {
      var r = cvs.getBoundingClientRect();
      var e = ev.touches ? ev.touches[0] : ev;
      return {
        x: (e.clientX - r.left) * cvs.width / r.width,
        y: (e.clientY - r.top) * cvs.height / r.height
      };
    }
    var stage = wrap.querySelector('.cc-fb-anno-stage');
    function placeText(ev) {
      var at = pos(ev);
      var sr = stage.getBoundingClientRect();
      var scale = cvs.getBoundingClientRect().width / cvs.width;
      var fz = Math.max(16, cvs.width / 36);
      var t = ev.touches ? ev.touches[0] : ev;
      var inp = document.createElement('input');
      inp.type = 'text';
      inp.className = 'cc-fb-anno-text';
      inp.placeholder = 'type…';
      inp.style.left = (t.clientX - sr.left) + 'px';
      inp.style.top = (t.clientY - sr.top) + 'px';
      inp.style.color = color;
      inp.style.fontSize = (fz * scale) + 'px';
      stage.appendChild(inp);
      setTimeout(function () { inp.focus(); }, 0);
      var done = false;
      function commit() {
        if (done) return;
        done = true;
        var v = inp.value.trim();
        inp.remove();
        if (v) { strokes.push({ tool: 'text', color: color, pts: [at], text: v, size: fz }); redraw(); }
      }
      inp.addEventListener('blur', commit);
      inp.addEventListener('keydown', function (e) {
        if (e.key === 'Enter') { e.preventDefault(); commit(); }
        else if (e.key === 'Escape') { inp.value = ''; commit(); }
      });
    }
    function down(ev) {
      ev.preventDefault();
      if (tool === 'text') { placeText(ev); return; }
      drawing = { tool: tool, color: color, pts: [pos(ev)] }; redraw();
    }
    function move(ev) {
      if (!drawing) return; ev.preventDefault();
      if (tool === 'pen') drawing.pts.push(pos(ev));
      else drawing.pts[1] = pos(ev);
      redraw();
    }
    function up() { if (drawing) { strokes.push(drawing); drawing = null; redraw(); } }

    cvs.addEventListener('mousedown', down);
    window.addEventListener('mousemove', move);
    window.addEventListener('mouseup', up);
    cvs.addEventListener('touchstart', down, { passive: false });
    cvs.addEventListener('touchmove', move, { passive: false });
    cvs.addEventListener('touchend', up);

    function cleanup() {
      window.removeEventListener('mousemove', move);
      window.removeEventListener('mouseup', up);
      wrap.remove();
    }

    wrap.addEventListener('click', function (ev) {
      var t = ev.target.closest('button');
      if (!t) return;
      if (t.dataset.tool) {
        tool = t.dataset.tool;
        wrap.querySelectorAll('[data-tool]').forEach(function (b) { b.classList.toggle('on', b === t); });
      } else if (t.dataset.color) {
        color = t.dataset.color;
        wrap.querySelectorAll('.cc-fb-anno-sw').forEach(function (b) { b.classList.toggle('on', b === t); });
      } else if (t.dataset.act === 'undo') {
        strokes.pop(); redraw();
      } else if (t.dataset.act === 'clear') {
        strokes = []; redraw();
      } else if (t.dataset.act === 'cancel') {
        cleanup();
      } else if (t.dataset.act === 'done') {
        var out = document.createElement('canvas');
        out.width = cvs.width; out.height = cvs.height;
        out.getContext('2d').drawImage(cvs, 0, 0);
        box.__shot = out;
        showShot(box);
        cleanup();
      }
    });

    redraw();
  }

  /* ---- events -------------------------------------------------------- */
  document.addEventListener('shown.bs.modal', function (ev) {
    var box = ev.target;
    if (!box || !box.querySelector) return;
    var fb = box.querySelector('.cc-feedback-modal');
    if (fb) capture(fb);
    // a maplibre widget inside a modal renders after the modal is shown (a
    // Shiny output round-trip) and inits before the dialog is fully sized --
    // poll for it and resize a few times (Depth Profile transect map)
    var tries = 0;
    var iv = setInterval(function () {
      box.querySelectorAll('.html-widget[id], .maplibregl-map[id]').forEach(function (el) {
        try {
          var inst = window.HTMLWidgets && HTMLWidgets.find('#' + el.id);
          var m = inst && (inst.getMap ? inst.getMap() : (inst.getMaps ? inst.getMaps()[0] : null));
          if (m && m.resize) { m.resize(); m.triggerRepaint && m.triggerRepaint(); }
        } catch (e) {}
      });
      if (++tries >= 16) clearInterval(iv);
    }, 350);
  });

  document.addEventListener('change', function (ev) {
    if (ev.target && ev.target.id === 'fb_include') {
      var shot = document.querySelector('#shiny-modal .cc-fb-shot');
      if (shot) shot.classList.toggle('off', !ev.target.checked);
    }
  });

  document.addEventListener('click', function (ev) {
    var root = document.getElementById('shiny-modal');
    var box = root && root.querySelector('.cc-feedback-modal');
    if (!box) return;
    // footer (status + buttons) is a sibling of .modal-body -> read from #shiny-modal
    var status = root.querySelector('.cc-fb-status');
    if (!status) return;

    if (ev.target.closest('.cc-fb-capture')) { ev.preventDefault(); grabScreen(box); return; }
    if (ev.target.closest('.cc-fb-retake'))  { ev.preventDefault(); grabScreen(box); return; }
    if (ev.target.closest('.cc-fb-edit'))    { ev.preventDefault(); annotate(box); return; }

    if (ev.target.closest('.cc-fb-gh')) {
      ev.preventDefault();
      var p = payload(box);
      var body = '**View:** ' + p.url + '\n**Release:** ' + p.release +
        ' · ' + p.viewport + ' · ' + p.theme + '\n\n' +
        (p.text || '_What happened / what did you expect?_') + '\n';
      window.open('https://github.com/CalCOFI/db-viz-hex/issues/new?labels=feedback&body=' +
        encodeURIComponent(body), '_blank', 'noopener');
      return;
    }

    var btn = ev.target.closest('.cc-fb-send');
    if (!btn) return;
    var endpoint = box.dataset.endpoint || '';
    var p2 = payload(box);
    if (!p2.text) {
      status.textContent = 'Add a note first.';
      status.className = 'cc-fb-status warn'; return;
    }
    if (!endpoint) {
      status.textContent = 'No feedback endpoint set — use the GitHub link.';
      status.className = 'cc-fb-status warn'; return;
    }
    btn.disabled = true;
    status.textContent = 'Sending…'; status.className = 'cc-fb-status';
    fetch(endpoint, {
      method: 'POST',
      headers: { 'Content-Type': 'text/plain;charset=UTF-8' },
      body: JSON.stringify(p2)
    }).then(function (r) {
      return r.json().catch(function () { return null; });
    }).then(function (j) {
      if (j && j.ok === false) throw new Error(j.error || 'the endpoint refused it');
      var link = (j && j.issue_url)
        ? ' and as <a href="' + j.issue_url + '" target="_blank" rel="noopener">a public issue</a>'
        : '';
      box.querySelector('.cc-fb-grid').innerHTML =
        '<p class="cc-fb-thanks">Thank you — the team gets it by mail' + link + '.</p>';
      box.classList.add('cc-fb-done');
      status.textContent = ''; btn.remove();
    }).catch(function (err) {
      btn.disabled = false;
      status.textContent = 'Not sent: ' + err.message + '. Try the GitHub link.';
      status.className = 'cc-fb-status warn';
    });
  });
})();
