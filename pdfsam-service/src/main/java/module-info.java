/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/09/22
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
module org.pdfsam.service {
    requires com.fasterxml.jackson.datatype.jdk8;
    requires com.fasterxml.jackson.datatype.jsr310;
    requires javafx.graphics;
    requires org.apache.commons.lang3;
    requires org.pdfsam.core;
    requires org.pdfsam.i18n;
    requires org.pdfsam.persistence;
    requires org.sejda.commons;
    requires org.sejda.impl.sambox;
    requires org.sejda.io;
    requires org.sejda.sambox;
    requires org.slf4j;

    requires transitive com.fasterxml.jackson.databind;
    requires transitive jakarta.inject;
    requires transitive org.pdfsam.eventstudio;
    requires transitive org.pdfsam.injector;
    requires transitive org.pdfsam.model;
    requires transitive org.sejda.core;
    requires transitive org.sejda.model;
    requires static org.sejda.core.writer;

    exports org.pdfsam.service.news;
    exports org.pdfsam.service.pdf;
    exports org.pdfsam.service.premium;
    exports org.pdfsam.service.task;
    exports org.pdfsam.service.tool;
    exports org.pdfsam.service.ui;
    exports org.pdfsam.service.update;

    opens org.pdfsam.service.ui to org.pdfsam.eventstudio, com.fasterxml.jackson.databind;
    opens org.pdfsam.service.tool to com.fasterxml.jackson.databind;
}