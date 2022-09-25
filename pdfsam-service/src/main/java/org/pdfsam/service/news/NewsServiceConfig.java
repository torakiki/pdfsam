/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06 nov 2016
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
package org.pdfsam.service.news;

import jakarta.inject.Named;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Provides;
import org.pdfsam.persistence.PreferencesRepository;

/**
 * @author Andrea Vacondio
 */
@Components({ LatestNewsController.class })
public class NewsServiceConfig {

    @Provides
    NewsService news(DefaultNewsService news) {
        return news;
    }

    @Provides
    @Named("newsRepository")
    PreferencesRepository newsRepo() {
        return new PreferencesRepository("/org/pdfsam/user/news");
    }
}
