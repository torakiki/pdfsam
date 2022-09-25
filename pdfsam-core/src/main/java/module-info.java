/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/09/22
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
module org.pdfsam.core {

    requires org.pdfsam.i18n;
    requires org.pdfsam.persistence;
    requires org.sejda.commons;
    requires org.slf4j;

    requires transitive io.reactivex.rxjava3;
    requires transitive java.xml;
    requires transitive javafx.graphics;
    requires transitive org.apache.commons.lang3;
    requires transitive org.pdfsam.injector;
    requires transitive org.pdfsam.model;
    requires transitive org.pdfsam.themes;
    requires transitive org.sejda.conversion;
    requires transitive org.sejda.model;

    exports org.pdfsam.core;
    exports org.pdfsam.core.context;
    exports org.pdfsam.core.support;
    exports org.pdfsam.core.support.io;
    exports org.pdfsam.core.support.params;
    exports org.pdfsam.core.support.validation;
}