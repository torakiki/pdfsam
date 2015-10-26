/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 ott 2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * JavaBean used as data model for a single news and used to unmashal a json
 * 
 * @author Andrea Vacondio
 */
public class NewsData {

    private int id;
    private String title;
    private String content;
    private String link;
    private LocalDate date;

    public int getId() {
        return id;
    }

    public void setId(String id) {
        this.id = Integer.valueOf(id);
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(String dateString) {
        this.date = LocalDate.parse(dateString, DateTimeFormatter.BASIC_ISO_DATE);
    }

}
