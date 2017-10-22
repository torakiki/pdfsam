/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20 set 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.news;

import org.pdfsam.support.RequireUtils;

/**
 * Notifies that a new important news is available
 * 
 * @author Andrea Vacondio
 *
 */
public class NewImportantNewsEvent {

    public final NewsData news;

    public NewImportantNewsEvent(NewsData news) {
        RequireUtils.requireNotNull(news, "News cannot be null");
        this.news = news;
    }
}
