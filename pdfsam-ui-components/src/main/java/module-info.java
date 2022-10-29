/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/10/22
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
module org.pdfsam.ui.components {
    requires org.apache.commons.io;
    requires org.kordamp.ikonli.javafx;
    requires org.kordamp.ikonli.unicons;
    requires org.pdfsam.i18n;
    requires org.pdfsam.injector;
    requires org.sejda.commons;
    requires org.slf4j;

    requires transitive javafx.base;
    requires transitive javafx.controls;
    requires transitive javafx.graphics;
    requires transitive org.apache.commons.lang3;
    requires transitive org.kordamp.ikonli.core;
    requires transitive org.pdfsam.core;
    requires transitive org.pdfsam.eventstudio;
    requires transitive org.pdfsam.model;
    requires transitive org.pdfsam.persistence;
    requires transitive org.sejda.conversion;
    requires transitive org.sejda.model;

    exports org.pdfsam.ui.components.commons;
    exports org.pdfsam.ui.components.io;
    exports org.pdfsam.ui.components.notification;
    exports org.pdfsam.ui.components.prefix;
    exports org.pdfsam.ui.components.selection.multiple;
    exports org.pdfsam.ui.components.selection.single;
    exports org.pdfsam.ui.components.support;
    exports org.pdfsam.ui.components.tool;

    opens org.pdfsam.ui.components.io to org.pdfsam.eventstudio;
    opens org.pdfsam.ui.components.tool to org.pdfsam.eventstudio;
    opens org.pdfsam.ui.components.selection.multiple to org.pdfsam.eventstudio;
    opens org.pdfsam.ui.components.selection.single to org.pdfsam.eventstudio;
}