/*
 * This file is part of the PDF Split And Merge source code
 * Created on 6 feb 2021
 * Copyright 2021 by Sober Lemur S.r.l. (info@soberlemur.com).
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
/**
 * @author Andrea Vacondio
 *
 */
module org.pdfsam.persistence {
    exports org.pdfsam.persistence;

    requires transitive com.fasterxml.jackson.databind;
    requires java.prefs;
    requires org.sejda.commons;
    requires org.slf4j;
}