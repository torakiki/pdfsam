/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/10/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
module org.pdfsam.gui {

    requires com.fasterxml.jackson.annotation;
    requires com.fasterxml.jackson.datatype.jdk8;
    requires com.fasterxml.jackson.datatype.jsr310;
    requires java.desktop;
    requires javafx.media;
    requires org.apache.commons.io;
    requires org.apache.commons.lang3;
    requires org.kordamp.ikonli.boxicons;
    requires org.kordamp.ikonli.javafx;
    requires org.kordamp.ikonli.unicons;
    requires org.pdfsam.i18n;
    requires org.pdfsam.persistence;
    requires org.pdfsam.service;
    requires org.sejda.commons;
    requires org.sejda.core;
    requires org.sejda.impl.sambox;
    requires org.slf4j;

    requires transitive ch.qos.logback.classic;
    requires transitive ch.qos.logback.core;
    requires transitive com.fasterxml.jackson.databind;
    requires transitive jakarta.inject;
    requires transitive javafx.base;
    requires transitive javafx.controls;
    requires transitive javafx.graphics;
    requires transitive org.kordamp.ikonli.core;
    requires transitive org.pdfsam.core;
    requires transitive org.pdfsam.eventstudio;
    requires transitive org.pdfsam.injector;
    requires transitive org.pdfsam.model;
    requires transitive org.pdfsam.themes;
    requires transitive org.pdfsam.ui.components;
    requires transitive org.sejda.model;

    exports org.pdfsam.gui;
    exports org.pdfsam.gui.components to org.pdfsam.eventstudio;
    exports org.pdfsam.gui.components.notification to org.pdfsam.eventstudio;
    exports org.pdfsam.gui.components.dialog to org.pdfsam.eventstudio;
    exports org.pdfsam.gui.components.dnd to org.pdfsam.eventstudio;
    exports org.pdfsam.gui.components.news to org.pdfsam.injector, org.pdfsam.eventstudio;

    opens org.pdfsam.gui.components to org.pdfsam.injector;
    opens org.pdfsam.gui.components.dashboard to org.pdfsam.injector, org.pdfsam.eventstudio;
    opens org.pdfsam.gui.components.dashboard.tools to org.pdfsam.injector, org.pdfsam.eventstudio;
    opens org.pdfsam.gui.components.dashboard.preference to org.pdfsam.injector;
    opens org.pdfsam.gui.components.info to org.pdfsam.injector, org.pdfsam.eventstudio;
    opens org.pdfsam.gui.components.workarea to org.pdfsam.injector, org.pdfsam.eventstudio;
    opens org.pdfsam.gui.components.log to org.pdfsam.injector;
    opens org.pdfsam.gui.components.banner to org.pdfsam.injector, org.pdfsam.eventstudio;
}