/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/09/22
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
module org.pdfsam.model {
    requires org.apache.commons.io;
    requires org.apache.commons.lang3;
    requires org.pdfsam.i18n;
    requires org.sejda.conversion;
    requires org.kordamp.ikonli.unicons;

    requires transitive org.kordamp.ikonli.core;
    requires transitive jakarta.validation;
    requires transitive javafx.base;
    requires transitive javafx.graphics;
    requires transitive org.sejda.commons;
    requires transitive org.sejda.model;

    exports org.pdfsam.model;
    exports org.pdfsam.model.io;
    exports org.pdfsam.model.lifecycle;
    exports org.pdfsam.model.news;
    exports org.pdfsam.model.pdf;
    exports org.pdfsam.model.premium;
    exports org.pdfsam.model.task;
    exports org.pdfsam.model.tool;
    exports org.pdfsam.model.ui;
    exports org.pdfsam.model.ui.workspace;
    exports org.pdfsam.model.update;

}