#!/usr/bin/python3
# -*- coding: utf-8 -*-
import os
import re
from gi.repository import Gtk, Gdk, Pango, PangoCairo, GLib


class NagWindow(Gtk.Window):
    def enter_notify_event(self, *args, **kwargs):
        self.move_to_other()

    def draw(self, widget, context):
        context.set_source_rgb(1.0, 0, 0)
        PangoCairo.show_layout(context, self.layout)

    def __init__(self, monitor_geometry):
        super().__init__(Gtk.WindowType.POPUP, border_width=0)

        self.darea = Gtk.DrawingArea()
        self.layout = self.darea.create_pango_layout("CLOCK")
        self.layout.set_font_description(Pango.FontDescription("Arial 150"))
        self.width, self.height = self.layout.get_pixel_size()

        self.add(self.darea)
        self.darea.show()
        self.darea.set_size_request(self.width, self.height)
        self.darea.realize()

        self.add_events(Gdk.EventType.ENTER_NOTIFY)
        self.darea.connect("draw", self.draw)
        self.connect("enter-notify-event", self.enter_notify_event)

        left_corner = monitor_geometry.x + monitor_geometry.width / 2 - self.width / 2
        top_corner_upper_position = monitor_geometry.y + self.height / 2
        top_corner_lower_position = monitor_geometry.y + monitor_geometry.height - self.height - self.height / 2

        self.current_position = 0
        self.valid_positions = [
            [left_corner, top_corner_upper_position],
            [left_corner, top_corner_lower_position]
        ]

        self.move_to_other()
        GLib.timeout_add_seconds(1, self.timer)

    def timer(self, *args, **kwargs):
        if not re.search('screen locked', os.popen("xscreensaver-command -time").read()):
            self.move_to_other()
        return True

    def move_to_other(self):
        self.current_position = (self.current_position + 1) % 2
        self.move(*self.valid_positions[self.current_position])
        self.present()

def main():
    if os.fork() != 0:
        os._exit(0)
    os.setsid()
    if os.fork() != 0:
        os._exit(0)
    os.close(0)
    os.close(1)
    os.close(2)
    screen = Gdk.Screen.get_default()
    n_monitors = screen.get_n_monitors()
    for i in range(n_monitors):
        win = NagWindow(screen.get_monitor_geometry(i))
        win.show_all()
    Gtk.main()

if __name__ == "__main__":
    main()
