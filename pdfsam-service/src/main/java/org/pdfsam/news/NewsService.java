/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24 ott 2015
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

import java.util.List;

/**
 * Service for application news related features
 * 
 * @author Andrea Vacondio
 *
 */
public interface NewsService {

    /**
     * @return a list with the latest news
     */
    List<NewsData> getLatestNews();

    /**
     * @return the latest news id the user has seen or -1 if the news panel was never open
     */
    int getLatestNewsSeen();

    /**
     * sets the id of the latest news seen
     */
    void setLatestNewsSeen(int id);

    /**
     * @return the latest important news id the user has seen or -1 if the news panel was never open
     */
    int getLatestImportantNewsSeen();

    /**
     * sets the id of the latest important news seen
     */
    void setLatestImportantNewsSeen(int id);

    /**
     * clear all stored information about the latest news
     */
    void clear();
}
